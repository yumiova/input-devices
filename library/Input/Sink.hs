{-# LANGUAGE ViewPatterns #-}

module Input.Sink
  ( -- * Internal streams
    Stream ((:<)),
    -- * Control sinks
    Sink (Sink, runSink),
    send
    )
where

import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Divisible (Divisible (conquer, divide))
import Data.Int (Int32)
import Data.Word (Word16)
import System.Libevdev
  ( InputEvent
      ( InputEvent,
        inputEventCode,
        inputEventTime,
        inputEventType,
        inputEventValue
        ),
    Timeval (Timeval, timevalSec, timevalUsec)
    )

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
newtype Sink a = Sink {runSink :: Stream a}

instance Contravariant Sink where
  contramap f = Sink . contramap f . runSink

instance Divisible Sink where

  divide f bsink = Sink . divide f (runSink bsink) . runSink

  conquer = Sink conquer

send :: Word16 -> Word16 -> (a -> Int32) -> Sink a
send kind code f = Sink $ [] :< first
  where
    -- Time values are never used by the user device aspect of `libevdev`
    timeval = Timeval {timevalSec = 0, timevalUsec = 0}
    -- First pass will always fire an event: the first value ever
    first a = [event] :< rest current
      where
        current = f a
        event = InputEvent
          { inputEventTime = timeval,
            inputEventType = kind,
            inputEventCode = code,
            inputEventValue = current
            }
    -- Remaining passes will only fire events if the values have changed
    rest previous a
      | previous == current = [] :< rest previous
      | otherwise = [event] :< rest current
      where
        current = f a
        event = InputEvent
          { inputEventTime = timeval,
            inputEventType = kind,
            inputEventCode = code,
            inputEventValue = current
            }
