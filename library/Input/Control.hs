{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Input.Control
  ( Control ((:<)),
    event,
    stateful,
    control,
    Key (Key, unKey),
    key,
    Axis (Axis, unAxis)
    )
where

import Control.Applicative (liftA2)
import Data.Int (Int32)
import Data.Word (Word16)
import qualified Language.C.Inline as C (include, pure)
import System.Libevdev (InputEvent (inputEventCode, inputEventType, inputEventValue))

C.include "<stdint.h>"

C.include "<linux/input.h>"

infixr 5 :<

data Control a = a :< (InputEvent -> Control a)

instance Functor Control where
  fmap f ~(a :< as) = f a :< (fmap f <$> as)

instance Applicative Control where

  pure a = a :< const (pure a)

  ~(f :< fs) <*> ~(a :< as) = f a :< liftA2 (<*>) fs as

event :: Control (Maybe InputEvent)
event = Nothing :< pure . Just

stateful :: a -> Control (a -> a) -> Control a
stateful current ~(f :< fs) = increment :< stateful increment . fs
  where
    increment = f current

control :: a -> Control Word16 -> Control Word16 -> Control (Int32 -> a) -> Control a
control initial types codes fs =
  stateful initial (maybe (\_ _ _ -> id) k <$> event <*> types <*> codes <*> fs)
  where
    k delta type' code f current
      | inputEventType delta == type' && inputEventCode delta == code = f (inputEventValue delta)
      | otherwise = current

newtype Key = Key {unKey :: Int32}

key :: Word16 -> Control Key
key code = control (Key 0) (pure type') (pure code) (pure Key)
  where
    type' = [C.pure| uint16_t { EV_KEY } |]

newtype Axis = Axis {unAxis :: Int32}
