module System.Evdev.Time
  ( Timeval (Timeval, timevalSec, timevalUsec)
    )
where

import Foreign.C (CSUSeconds, CTime)

data Timeval = Timeval {timevalSec :: CTime, timevalUsec :: CSUSeconds}
  deriving (Eq, Ord, Show, Read)
