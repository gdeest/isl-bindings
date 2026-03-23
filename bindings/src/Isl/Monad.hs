{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}

module Isl.Monad
  ( Isl(..)
  , Ur(..)
  , runIsl
  , getCtx
  , unsafeIslFromIO
  , freeM
  ) where

import Control.DeepSeq (NFData, ($!!))
import Control.Exception (bracket, evaluate)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.Types

-- | Unrestricted wrapper. Wrapping a value in 'Ur' signals that it contains
-- no linear ISL resources and may be freely shared.
data Ur a where
  Ur :: a -> Ur a

-- | The ISL monad. Sequences ISL operations and carries the context.
-- Linearity is enforced on the /values/ (ISL objects), not on the bind —
-- we reuse standard 'Monad' for do-notation convenience.
newtype Isl a = Isl { unIsl :: Ctx -> IO a }

instance Functor Isl where
  fmap f (Isl g) = Isl $ \ctx -> fmap f (g ctx)

instance Applicative Isl where
  pure a = Isl $ \_ -> pure a
  Isl f <*> Isl a = Isl $ \ctx -> f ctx <*> a ctx

instance Monad Isl where
  Isl m >>= k = Isl $ \ctx -> do
    a <- m ctx
    unIsl (k a) ctx

-- | Run an ISL computation. The 'NFData' constraint ensures the return
-- value is fully evaluated before the ISL context is freed — preventing
-- any lazy thunks from referencing ISL memory after context cleanup.
runIsl :: NFData a => Isl (Ur a) -> a
runIsl (Isl f) = unsafePerformIO $
  bracket c_ctx_alloc c_ctx_free $ \ctxPtr -> do
    Ur a <- f (Ctx ctxPtr)
    return $!! a

-- | Retrieve the ISL context. Used by generated code.
getCtx :: Isl Ctx
getCtx = Isl $ \ctx -> return ctx

-- | Lift a raw IO action (that needs the context) into 'Isl'.
-- Used by generated code for FFI calls.
unsafeIslFromIO :: (Ctx -> IO a) -> Isl a
unsafeIslFromIO = Isl

-- | Free an ISL object within the Isl monad. Unlike 'consume' (which uses
-- unsafePerformIO and can be deferred by lazy evaluation), 'freeM' is
-- sequenced by the monad — the free happens in-order before subsequent actions.
freeM :: Consumable a => a -> Isl ()
freeM x = Isl $ \_ -> evaluate (consume x)
