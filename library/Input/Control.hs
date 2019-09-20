{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Input.Control
  ( Key (Key, unKey),
    key,
    Axis (Axis, unAxis),
    axis,
    Joystick (Joystick, joystickX, joystickY),
    joystick
    )
where

import Data.Int (Int32)
import Data.Word (Word16)
import Input.Source (Source, subscribe)
import qualified Language.C.Inline as C (include, pure)

C.include "<stdint.h>"

C.include "<linux/input.h>"

newtype Key = Key {unKey :: Int32}
  deriving (Eq, Ord, Show, Read)

key :: Word16 -> Source Key
key code = subscribe kind code Key (Key 0)
  where
    kind = [C.pure| uint16_t { EV_KEY } |]

newtype Axis = Axis {unAxis :: Int32}
  deriving (Eq, Ord, Show, Read)

axis :: Word16 -> Source Axis
axis code = subscribe kind code Axis (Axis 0)
  where
    kind = [C.pure| uint16_t { EV_ABS } |]

data Joystick = Joystick {joystickX :: Axis, joystickY :: Axis}
  deriving (Eq, Ord, Show, Read)

joystick :: Word16 -> Word16 -> Source Joystick
joystick xCode yCode = Joystick <$> axis xCode <*> axis yCode
