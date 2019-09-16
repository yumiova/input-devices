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
        ),
    libevdevCtx
    )
where

import Language.C.Inline.Context (Context)
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
    InputEvent (InputEvent, inputEventCode, inputEventTime, inputEventType, inputEventValue),
    inputCtx
    )
import System.Evdev.Time
  ( Timeval (Timeval, timevalSec, timevalUsec)
    )

libevdevCtx :: Context
libevdevCtx = inputCtx
