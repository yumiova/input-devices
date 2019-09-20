{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Input.Control
  ( Key (Key, unKey),
    key,
    Axis (Axis, axisAbsinfo, axisValue),
    axis,
    Joystick (Joystick, joystickX, joystickY),
    joystick
    )
where

import Data.Int (Int32)
import Data.Word (Word16)
import Input.Source (Source, receive, receiveAbs)
import qualified Language.C.Inline as C (include, pure)
import System.Libevdev (InputAbsinfo)

C.include "<stdint.h>"

C.include "<linux/input.h>"

newtype Key = Key {unKey :: Int32}
  deriving (Eq, Ord, Show, Read)

key :: Word16 -> Source Key
key code = receive kind code Key (Key 0)
  where
    kind = [C.pure| uint16_t { EV_KEY } |]

data Axis = Axis {axisAbsinfo :: InputAbsinfo, axisValue :: Int32}
  deriving (Eq, Ord, Show, Read)

axis :: Word16 -> Source Axis
axis code = receiveAbs kind code Axis (`Axis` 0)
  where
    kind = [C.pure| uint16_t { EV_ABS } |]

data Joystick = Joystick {joystickX :: Axis, joystickY :: Axis}
  deriving (Eq, Ord, Show, Read)

joystick :: Word16 -> Word16 -> Source Joystick
joystick xCode yCode = Joystick <$> axis xCode <*> axis yCode
