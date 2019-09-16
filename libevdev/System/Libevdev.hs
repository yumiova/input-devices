{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Libevdev
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
    Libevdev,
    LibevdevUinput,
    libevdevCtx
    )
where

import qualified Data.Map as Map (fromList)
import Language.C.Inline.Context (Context (ctxTypesTable))
import Language.C.Types (TypeSpecifier (Struct))
import System.Libevdev.Input
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
import System.Libevdev.Time
  ( Timeval (Timeval, timevalSec, timevalUsec)
    )

data Libevdev

data LibevdevUinput

libevdevCtx :: Context
libevdevCtx =
  inputCtx
    <> mempty
      { ctxTypesTable =
          Map.fromList
            [ (Struct "libevdev", [t|Libevdev|]),
              (Struct "libevdev_uinput", [t|LibevdevUinput|])
              ]
        }
