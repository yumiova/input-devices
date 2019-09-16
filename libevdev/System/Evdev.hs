{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module System.Evdev
  ( Timeval (Timeval, timevalSec, timevalUsec),
    InputEvent (InputEvent, inputEventTime, inputEventType, inputEventCode, inputEventValue),
    InputAbsinfo
      ( InputAbsinfo,
        inputAbsinfoValue,
        inputAbsinfoMinimum,
        inputAbsinfoMaximum,
        inputAbsinfoFuzz,
        inputAbsinfoFlat,
        inputAbsinfoResolution
        ),
    evdevCtx
    )
where

import Data.Int (Int32)
import qualified Data.Map as Map (fromList)
import Data.Word (Word16)
import Foreign (Ptr, Storable (alignment, peek, poke, sizeOf), alloca, castPtr)
import Foreign.C (CInt (CInt))
import qualified Language.C.Inline as C (baseCtx, block, context, include, pure, withPtrs_)
import Language.C.Inline.Context (Context (ctxTypesTable))
import Language.C.Types (TypeSpecifier (Struct))
import System.Evdev.Time
  ( Timeval (Timeval, timevalSec, timevalUsec),
    timevalCtx
    )
import Prelude hiding (maximum, minimum)

C.context (C.baseCtx <> timevalCtx)

C.include "<stdint.h>"

C.include "<linux/input.h>"

marshal :: Storable a => a -> (Ptr a -> IO b) -> IO b
marshal a f =
  alloca $ \ptr -> do
    poke ptr a
    f ptr

data InputEvent
  = InputEvent
      { inputEventTime :: Timeval,
        inputEventType :: Word16,
        inputEventCode :: Word16,
        inputEventValue :: Int32
        }
  deriving (Eq, Ord, Show, Read)

instance Storable InputEvent where

  sizeOf _ = fromIntegral [C.pure| int { sizeof(struct input_event) } |]

  alignment _ = fromIntegral [C.pure| int { __alignof__(struct input_event) } |]

  peek (castPtr -> source) = do
    (time, type', code, value) <-
      C.withPtrs_ $ \(timePtr, typePtr, codePtr, valuePtr) ->
        [C.block| void {
          struct input_event *target = (struct input_event *) $(void *source);
          *$(struct timeval *timePtr) = target->time;
          *$(uint16_t *typePtr) = target->type;
          *$(uint16_t *codePtr) = target->code;
          *$(int32_t *valuePtr) = target->value;
        } |]
    pure (InputEvent time type' code value)

  poke (castPtr -> target) (InputEvent time type' code value) =
    marshal time $ \timePtr ->
      [C.block| void {
        struct input_event *target = (struct input_event *) $(void *target);
        target->time = *$(struct timeval *timePtr);
        target->type = $(uint16_t type');
        target->code = $(uint16_t code);
        target->value = $(int32_t value);
      } |]

data InputAbsinfo
  = InputAbsinfo
      { inputAbsinfoValue :: Int32,
        inputAbsinfoMinimum :: Int32,
        inputAbsinfoMaximum :: Int32,
        inputAbsinfoFuzz :: Int32,
        inputAbsinfoFlat :: Int32,
        inputAbsinfoResolution :: Int32
        }
  deriving (Eq, Ord, Show, Read)

instance Storable InputAbsinfo where

  sizeOf _ = fromIntegral [C.pure| int { sizeof(struct input_absinfo) } |]

  alignment _ = fromIntegral [C.pure| int { __alignof__(struct input_absinfo) } |]

  peek (castPtr -> source) = do
    (value, minimum, maximum, fuzz, flat, resolution) <-
      C.withPtrs_ $ \(value, minimum, maximum, fuzz, flat, resolution) ->
        [C.block| void {
          struct input_absinfo *target = (struct input_absinfo *) $(void *source);
          *$(int32_t *value) = target->value;
          *$(int32_t *minimum) = target->minimum;
          *$(int32_t *maximum) = target->maximum;
          *$(int32_t *fuzz) = target->fuzz;
          *$(int32_t *flat) = target->flat;
          *$(int32_t *resolution) = target->resolution;
        } |]
    pure (InputAbsinfo value minimum maximum fuzz flat resolution)

  poke (castPtr -> target) (InputAbsinfo value minimum maximum fuzz flat resolution) =
    [C.block| void {
      struct input_absinfo *target = (struct input_absinfo *) $(void *target);
      target->value = $(int32_t value);
      target->minimum = $(int32_t minimum);
      target->maximum = $(int32_t maximum);
      target->fuzz = $(int32_t fuzz);
      target->flat = $(int32_t flat);
      target->resolution = $(int32_t resolution);
    } |]

evdevCtx :: Context
evdevCtx =
  timevalCtx
    <> mempty
      { ctxTypesTable =
          Map.fromList
            [ (Struct "input_event", [t|InputEvent|]),
              (Struct "input_absinfo", [t|InputAbsinfo|])
              ]
        }
