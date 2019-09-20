module Input.Sink
  ( -- * Internal streams
    Stream ((:<))
    )
where

import System.Libevdev (InputEvent)

-- * Internal streams
infixr 5 :<

data Stream a = [InputEvent] :< (a -> Stream a)
