{-# LANGUAGE ViewPatterns #-}

module Input.Sink
  ( -- * Internal streams
    Stream ((:<))
    )
where

import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Divisible (Divisible (conquer, divide))
import System.Libevdev (InputEvent)

-- * Internal streams
infixr 5 :<

data Stream a = [InputEvent] :< (a -> Stream a)

instance Contravariant Stream where
  contramap f ~(bevent :< bs) = bevent :< contramap f . bs . f

instance Divisible Stream where

  divide f ~(bevent :< bs) ~(cevent :< cs) =
    bevent <> cevent :< \(f -> ~(b, c)) -> divide f (bs b) (cs c)

  conquer = [] :< const conquer
