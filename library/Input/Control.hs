module Input.Control
  ( Control ((:<))
    )
where

import Control.Applicative (liftA2)
import System.Libevdev (InputEvent)

infixr 5 :<

data Control a = a :< (InputEvent -> Control a)

instance Functor Control where
  fmap f ~(a :< as) = f a :< (fmap f <$> as)

instance Applicative Control where

  pure a = a :< const (pure a)

  ~(f :< fs) <*> ~(a :< as) = f a :< liftA2 (<*>) fs as
