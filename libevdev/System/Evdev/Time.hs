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
import qualified Language.C.Inline as C (block, exp, include, pure)
import Language.C.Inline.Context (Context (ctxTypesTable))
import Language.C.Types (TypeSpecifier (Struct))

C.include "<sys/time.h>"

data Timeval = Timeval {timevalSec :: CTime, timevalUsec :: CSUSeconds}
  deriving (Eq, Ord, Show, Read)

instance Storable Timeval where

  sizeOf _ = fromIntegral [C.pure| int { sizeof(struct timeval) } |]

  alignment _ = fromIntegral [C.pure| int { __alignof__(struct timeval) } |]

  peek (castPtr -> source) =
    Timeval
      <$> [C.exp| time_t {
            ((struct timeval *) $(void *source))->tv_sec
          } |]
      <*> [C.exp| suseconds_t {
            ((struct timeval *) $(void *source))->tv_usec
          } |]

  poke (castPtr -> target) (Timeval sec usec) =
    [C.block| void {
      ((struct timeval *) $(void *target))->tv_sec = $(time_t sec);
      ((struct timeval *) $(void *target))->tv_usec = $(suseconds_t usec);
    } |]

timevalCtx :: Context
timevalCtx =
  mempty {ctxTypesTable = Map.singleton (Struct "timeval") [t|Timeval|]}
