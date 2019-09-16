{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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
    Libevdev,
    libevdevCtx
    )
where

import qualified Data.Map as Map (singleton)
import Language.C.Inline.Context (Context (ctxTypesTable))
import Language.C.Types (TypeSpecifier (Struct))
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

data Libevdev

libevdevCtx :: Context
libevdevCtx =
  inputCtx
    <> mempty {ctxTypesTable = Map.singleton (Struct "libevdev") [t|Libevdev|]}
