{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module System.Evdev
  ( Timeval (Timeval, timevalSec, timevalUsec),
    InputEvent
      ( InputEvent,
        inputEventTime,
        inputEventType,
        inputEventCode,
        inputEventValue
        ),
    evdevCtx
    )
where

import Data.Int (Int32)
import qualified Data.Map as Map (singleton)
import Data.Word (Word16)
import Foreign (Ptr, Storable (alignment, peek, poke, sizeOf), alloca, castPtr)
import Foreign.C (CInt (CInt))
import qualified Language.C.Inline as C
  ( baseCtx,
    block,
    context,
    exp,
    include,
    pure,
    withPtr_
    )
import Language.C.Inline.Context (Context (ctxTypesTable))
import Language.C.Types (TypeSpecifier (Struct))
import System.Evdev.Time
  ( Timeval (Timeval, timevalSec, timevalUsec),
    timevalCtx
    )

C.context (C.baseCtx <> timevalCtx)

C.include "<stdint.h>"

C.include "<linux/input.h>"

data InputEvent
  = InputEvent
      { inputEventTime :: Timeval,
        inputEventType :: Word16,
        inputEventCode :: Word16,
        inputEventValue :: Int32
        }
  deriving (Eq, Ord, Show, Read)

marshal :: Storable a => a -> (Ptr a -> IO b) -> IO b
marshal a f =
  alloca $ \ptr -> do
    poke ptr a
    f ptr

instance Storable InputEvent where

  sizeOf _ = fromIntegral [C.pure| int { sizeof(struct input_event) } |]

  alignment _ = fromIntegral [C.pure| int { __alignof__(struct input_event) } |]

  peek (castPtr -> source) = InputEvent <$> time <*> type' <*> code <*> value
    where
      time =
        C.withPtr_ $ \out ->
          [C.block| void {
            *$(struct timeval *out) =
              ((struct input_event *) $(void *source))->time;
          } |]
      type' =
        [C.exp| uint16_t { ((struct input_event *) $(void *source))->type } |]
      code =
        [C.exp| uint16_t { ((struct input_event *) $(void *source))->code } |]
      value =
        [C.exp| int32_t { ((struct input_event *) $(void *source))->value } |]

  poke (castPtr -> target) (InputEvent time type' code value) =
    marshal time $ \timePtr ->
      [C.block| void {
        struct input_event *target = (struct input_event *) $(void *target);
        target->time = *$(struct timeval *timePtr);
        target->type = $(uint16_t type');
        target->code = $(uint16_t code);
        target->value = $(int32_t value);
      } |]

evdevCtx :: Context
evdevCtx =
  timevalCtx
    <> mempty
      { ctxTypesTable = Map.singleton (Struct "input_event") [t|InputEvent|]
        }
