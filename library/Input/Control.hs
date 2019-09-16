module Input.Control
  ( Control ((:<))
    )
where

import System.Libevdev (InputEvent)

infixr 5 :<

data Control a = a :< (InputEvent -> Control a)
