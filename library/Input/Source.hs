{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Input.Source
  ( -- * Internal streams
    Stream ((:<)),
    -- * Control sources
    Source (Source, runSource),
    event,
    stateful,
    control,
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
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import qualified Language.C.Inline as C (include, pure)
import System.Libevdev (InputEvent (inputEventCode, inputEventType, inputEventValue))

C.include "<stdint.h>"

C.include "<linux/input.h>"

infixr 5 :<

-- * Internal streams
data Stream a = a :< (InputEvent -> Stream a)

instance Functor Stream where
  fmap f ~(a :< as) = f a :< (fmap f <$> as)

instance Applicative Stream where

  pure a = a :< const (pure a)

  ~(f :< fs) <*> ~(a :< as) = f a :< liftA2 (<*>) fs as

-- * Control sources
newtype Source a = Source {runSource :: Stream a}

instance Functor Source where
  fmap f = Source . fmap f . runSource

instance Applicative Source where

  pure = Source . pure

  (<*>) source = Source . (<*>) (runSource source) . runSource

event :: Source (Maybe InputEvent)
event = Source go
  where
    go = Nothing :< \current -> Just . fromMaybe current <$> go

stateful :: a -> Source (a -> a) -> Source a
stateful initial = Source . go initial . runSource
  where
    go current ~(f :< fs) = f current :< go (f current) . fs

control :: a -> Source Word16 -> Source Word16 -> Source (Int32 -> a) -> Source a
control initial types codes fs =
  stateful initial (maybe (\_ _ _ -> id) k <$> event <*> types <*> codes <*> fs)
  where
    k delta type' code f current
      | inputEventType delta == type' && inputEventCode delta == code = f (inputEventValue delta)
      | otherwise = current

-- * Controls
newtype Key = Key {unKey :: Int32}

key :: Word16 -> Source Key
key code = control (Key 0) (pure type') (pure code) (pure Key)
  where
    type' = [C.pure| uint16_t { EV_KEY } |]

newtype Axis = Axis {unAxis :: Int32}

axis :: Word16 -> Source Axis
axis code = control (Axis 0) (pure type') (pure code) (pure Axis)
  where
    type' = [C.pure| uint16_t { EV_ABS } |]

data Joystick = Joystick {joystickX :: Axis, joystickY :: Axis}

joystick :: Word16 -> Word16 -> Source Joystick
joystick xCode yCode = Joystick <$> axis xCode <*> axis yCode
