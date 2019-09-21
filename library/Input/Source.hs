{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

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

receiveWith
  :: (Ptr Libevdev -> IO b)
  -> Word16
  -> Word16
  -> (Int32 -> a)
  -> a
  -> Source (b, a)
receiveWith after kind code f initial =
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
      then do
        also <- after libevdev
        pure (Just ((also,) <$> stream))
      else pure Nothing
  where
    step current event
      | inputEventType event == kind && inputEventCode event == code =
        f (inputEventValue event)
      | otherwise = current
    loop current delta = increment :< loop increment
      where
        { increment = foldl' step current delta }
    stream = initial :< loop initial

receive :: Word16 -> Word16 -> (Int32 -> a) -> a -> Source a
receive kind code f = fmap snd . receiveWith (const (pure ())) kind code f

receiveAbs
  :: Word16
  -> Word16
  -> (Int32 -> a)
  -> a
  -> Source (InputAbsinfo, a)
receiveAbs kind code = receiveWith after kind code
  where
    after libevdev = do
      source <-
        [C.exp| struct input_absinfo const * {
          libevdev_get_abs_info($(struct libevdev *libevdev), $(uint16_t code))
        } |]
      peek source
