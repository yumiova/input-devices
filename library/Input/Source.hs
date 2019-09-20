{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Input.Source
  ( -- * Internal streams
    Stream ((:<)),
    -- * Control sources
    Source (Source, runSource),
    receive,
    receiveAbs
    )
where

import Control.Applicative (liftA2)
import Data.Foldable (foldl')
import Data.Int (Int32)
import Data.Word (Word16)
import Foreign (Ptr, peek)
import Foreign.C (CInt (CInt))
import qualified Language.C.Inline as C (baseCtx, context, exp, include)
import System.Libevdev
  ( InputAbsinfo,
    InputEvent (inputEventCode, inputEventType, inputEventValue),
    Libevdev,
    libevdevCtx
    )

C.context (C.baseCtx <> libevdevCtx)

C.include "<stdint.h>"

C.include "<libevdev/libevdev.h>"

-- * Internal streams
infixr 5 :<

data Stream a = a :< ([InputEvent] -> Stream a)

instance Functor Stream where
  fmap f ~(a :< as) = f a :< (fmap f <$> as)

instance Applicative Stream where

  pure a = a :< const (pure a)

  ~(f :< fs) <*> ~(a :< as) = f a :< liftA2 (<*>) fs as

-- * Control sources
newtype Source a = Source {runSource :: Ptr Libevdev -> IO (Maybe (Stream a))}

instance Functor Source where
  fmap f = Source . fmap (fmap (fmap (fmap f))) . runSource

instance Applicative Source where

  pure = Source . const . pure . Just . pure

  (<*>) source = Source . liftA2 k (runSource source) . runSource
    where
      k faction action = do
        mstream <- faction
        case mstream of
          Nothing -> pure Nothing
          Just stream -> fmap (stream <*>) <$> action

receive :: Word16 -> Word16 -> (Int32 -> a) -> a -> Source a
receive kind code f initial =
  Source $ \libevdev -> do
    valid <-
      [C.exp| int {
        libevdev_has_event_code(
          $(struct libevdev *libevdev),
          $(uint16_t kind),
          $(uint16_t code)
        )
      } |]
    if valid == 1
      then pure (Just (initial :< loop initial))
      else pure Nothing
  where
    apply current event
      | inputEventType event == kind && inputEventCode event == code =
        f (inputEventValue event)
      | otherwise = current
    loop current delta = increment :< loop increment
      where
        { increment = foldl' apply current delta }

absinfo :: Word16 -> Source InputAbsinfo
absinfo code =
  Source $ \libevdev -> do
    source <-
      [C.exp| struct input_absinfo const * {
        libevdev_get_abs_info($(struct libevdev *libevdev), $(uint16_t code))
      } |]
    value <- peek source
    pure (Just (pure value))

receiveAbs
  :: Word16
  -> Word16
  -> (InputAbsinfo -> Int32 -> a)
  -> (InputAbsinfo -> a)
  -> Source a
receiveAbs kind code f initial =
  receive kind code (flip f) initial <*> absinfo code
