{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module System.Evdev.Time
  ( Timeval (Timeval, timevalSec, timevalUsec),
    timevalCtx
    )
where

import qualified Data.Map as Map (singleton)
import Foreign (Storable (alignment, peek, poke, sizeOf), castPtr)
import Foreign.C (CInt (CInt), CSUSeconds (CSUSeconds), CTime (CTime))
import qualified Language.C.Inline as C (block, include, pure, withPtrs_)
import Language.C.Inline.Context (Context (ctxTypesTable))
import Language.C.Types (TypeSpecifier (Struct))

C.include "<sys/time.h>"

data Timeval = Timeval {timevalSec :: CTime, timevalUsec :: CSUSeconds}
  deriving (Eq, Ord, Show, Read)

instance Storable Timeval where

  sizeOf _ = fromIntegral [C.pure| int { sizeof(struct timeval) } |]

  alignment _ = fromIntegral [C.pure| int { __alignof__(struct timeval) } |]

  peek (castPtr -> source) = do
    (sec, usec) <-
      C.withPtrs_ $ \(secPtr, usecPtr) ->
        [C.block| void {
          struct timeval *timeval = (struct timeval *) $(void *source);
          *$(time_t *secPtr) = timeval->tv_sec;
          *$(suseconds_t *usecPtr) = timeval->tv_usec;
        } |]
    pure (Timeval sec usec)

  poke (castPtr -> target) (Timeval sec usec) =
    [C.block| void {
      struct timeval *timeval = (struct timeval *) $(void *target);
      timeval->tv_sec = $(time_t sec);
      timeval->tv_usec = $(suseconds_t usec);
    } |]

timevalCtx :: Context
timevalCtx =
  mempty {ctxTypesTable = Map.singleton (Struct "timeval") [t|Timeval|]}
