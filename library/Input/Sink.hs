{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Input.Sink
  ( -- * Internal streams
    Stream ((:<)),
    -- * Control sinks
    Sink (Sink, runSink),
    sendWith,
    send,
    sendAbs
    )
where

import Control.Applicative (liftA2)
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Divisible (Divisible (conquer, divide))
import Data.Int (Int32)
import Data.Profunctor (Profunctor (dimap, lmap))
import Data.Word (Word16)
import Foreign (Ptr, alloca, castPtr, nullPtr, poke)
import qualified Language.C.Inline as C (baseCtx, context, exp, include)
import System.Libevdev
  ( InputAbsinfo,
    InputEvent
      ( InputEvent,
        inputEventCode,
        inputEventTime,
        inputEventType,
        inputEventValue
        ),
    Libevdev,
    Timeval (Timeval, timevalSec, timevalUsec),
    libevdevCtx
    )

C.context (C.baseCtx <> libevdevCtx)

C.include "<stdint.h>"

C.include "<stdlib.h>"

C.include "<libevdev/libevdev.h>"

-- * Internal streams
infixr 5 :<

data Stream a = [InputEvent] :< (a -> Stream a)

instance Contravariant Stream where
  contramap f ~(bevent :< bs) = bevent :< contramap f . bs . f

instance Divisible Stream where

  divide f ~(bevent :< bs) ~(cevent :< cs) =
    bevent <> cevent :< \(f -> ~(b, c)) -> divide f (bs b) (cs c)

  conquer = [] :< const conquer

-- * Control sinks
newtype Sink a = Sink {runSink :: Ptr Libevdev -> a -> IO (Stream a)}

instance Contravariant Sink where
  contramap f = Sink . fmap (dimap f (fmap (contramap f))) . runSink

diliftA2
  :: (Profunctor p, Applicative (p (a, b)))
  => (c -> (a, b))
  -> (d -> e -> f)
  -> p a d
  -> p b e
  -> p c f
diliftA2 f g left = dimap f (uncurry g) . liftA2 (,) (lmap fst left) . lmap snd

instance Divisible Sink where

  divide f bsink =
    Sink . liftA2 (diliftA2 f (liftA2 (divide f))) (runSink bsink) . runSink

  conquer = Sink (const (const (pure conquer)))

sendWith
  :: (b -> (Ptr b -> IO ()) -> IO ())
  -> Word16
  -> Word16
  -> (a -> Int32)
  -> Sink (b, a)
sendWith before kind code f =
  Sink $ \libevdev (payload, a) -> do
    let current = f a
        event = InputEvent
          { inputEventTime = timeval,
            inputEventType = kind,
            inputEventCode = code,
            inputEventValue = current
            }
        stream = [event] :< loop current
    before payload $ \(castPtr -> pointer) ->
      [C.exp| void {
        libevdev_enable_event_code(
          $(struct libevdev *libevdev),
          $(uint16_t kind),
          $(uint16_t code),
          $(void *pointer)
        )
      } |]
    pure stream
  where
    -- Time values are never used by the user device aspect of `libevdev`
    timeval = Timeval {timevalSec = 0, timevalUsec = 0}
    loop previous (_, a)
      | previous == current = [] :< loop previous
      | otherwise = [event] :< loop current
      where
        current = f a
        event = InputEvent
          { inputEventTime = timeval,
            inputEventType = kind,
            inputEventCode = code,
            inputEventValue = current
            }

send :: Word16 -> Word16 -> (a -> Int32) -> Sink a
send kind code = contramap ((),) . sendWith (const ($ nullPtr)) kind code

sendAbs :: Word16 -> Word16 -> (a -> Int32) -> Sink (InputAbsinfo, a)
sendAbs = sendWith before
  where
    before a f =
      alloca $ \pointer -> do
        poke pointer a
        f pointer
