module System.Evdev
  ( Timeval (Timeval, timevalSec, timevalUsec),
    InputEvent (InputEvent, inputEventCode, inputEventTime, inputEventType, inputEventValue),
    InputAbsinfo
      ( InputAbsinfo,
        inputAbsinfoFlat,
        inputAbsinfoFuzz,
        inputAbsinfoMaximum,
        inputAbsinfoMinimum,
        inputAbsinfoResolution,
        inputAbsinfoValue
        )
    )
where

import System.Evdev.Input
  ( InputAbsinfo
      ( InputAbsinfo,
        inputAbsinfoFlat,
        inputAbsinfoFuzz,
        inputAbsinfoMaximum,
        inputAbsinfoMinimum,
        inputAbsinfoResolution,
        inputAbsinfoValue
        ),
    InputEvent (InputEvent, inputEventCode, inputEventTime, inputEventType, inputEventValue)
    )
import System.Evdev.Time
  ( Timeval (Timeval, timevalSec, timevalUsec)
    )
