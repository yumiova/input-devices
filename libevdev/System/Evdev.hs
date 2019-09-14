module System.Evdev
  ( Timeval (Timeval, timevalSec, timevalUsec),
    InputEvent
      ( InputEvent,
        inputEventTime,
        inputEventType,
        inputEventCode,
        inputEventValue
        )
    )
where

import Data.Int (Int32)
import Data.Word (Word16)
import System.Evdev.Time (Timeval (Timeval, timevalSec, timevalUsec))

data InputEvent
  = InputEvent
      { inputEventTime :: Timeval,
        inputEventType :: Word16,
        inputEventCode :: Word16,
        inputEventValue :: Int32
        }
  deriving (Eq, Ord, Show, Read)
