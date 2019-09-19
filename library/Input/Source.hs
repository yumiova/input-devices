{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Input.Source
  ( -- * Internal streams
    Stream ((:<)),
    -- * Control sources
    Source (Source, runSource),
    subscribe,
    -- * Controls
    Key (Key, unKey),
    key,
    Axis (Axis, unAxis),
    axis,
    Joystick (Joystick, joystickX, joystickY),
    joystick
    )
where

import Control.Applicative (liftA2)
import Data.Int (Int32)
import Data.Word (Word16)
import Foreign (Ptr)
import qualified Language.C.Inline as C (include, pure)
import System.Libevdev (InputEvent (inputEventCode, inputEventType, inputEventValue), Libevdev)

C.include "<stdint.h>"

C.include "<linux/input.h>"

-- * Internal streams
infixr 5 :<

data Stream a = a :< (InputEvent -> Stream a)

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

  (<*>) source = Source . liftA2 (liftA2 (liftA2 (<*>))) (runSource source) . runSource

subscribe :: Word16 -> Word16 -> (Int32 -> a) -> a -> Source a
subscribe kind code f initial = Source (const (pure (Just (initial :< loop initial))))
  where
    loop current event
      | inputEventType event == kind && inputEventCode event == code =
        let { increment = f (inputEventValue event) }
         in increment :< loop increment
      | otherwise = current :< loop current

-- * Controls
newtype Key = Key {unKey :: Int32}

key :: Word16 -> Source Key
key code = subscribe kind code Key (Key 0)
  where
    kind = [C.pure| uint16_t { EV_KEY } |]

newtype Axis = Axis {unAxis :: Int32}

axis :: Word16 -> Source Axis
axis code = subscribe kind code Axis (Axis 0)
  where
    kind = [C.pure| uint16_t { EV_ABS } |]

data Joystick = Joystick {joystickX :: Axis, joystickY :: Axis}

joystick :: Word16 -> Word16 -> Source Joystick
joystick xCode yCode = Joystick <$> axis xCode <*> axis yCode
