module Input.Sink
  ( -- * Internal streams
    Stream ((:<))
    )
where

import Data.Functor.Contravariant (Contravariant (contramap))
import System.Libevdev (InputEvent)

-- * Internal streams
infixr 5 :<

data Stream a = [InputEvent] :< (a -> Stream a)

instance Contravariant Stream where
  contramap f ~(bevent :< bs) = bevent :< contramap f . bs . f
