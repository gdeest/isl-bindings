{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}

module Isl.Monad
  ( IslT(..)
  , Isl
  , Ur(..)
  , runIslT
  , runIsl
  , getCtx
  , unsafeIslFromIO
  , freeM
  ) where

import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.Types

-- | Unrestricted wrapper. Wrapping a value in 'Ur' signals that it contains
-- no linear ISL resources and may be freely shared.
data Ur a where
  Ur :: a -> Ur a

-- | The ISL monad transformer. Carries an ISL context through a monadic
-- computation. Use 'IslT' to compose ISL operations with other effects
-- (state, error handling, etc.) in a transformer stack.
newtype IslT m a = IslT { unIslT :: Ctx -> m a }

-- | Concrete ISL monad — 'IslT' specialized to 'IO'.
type Isl = IslT IO

instance Functor m => Functor (IslT m) where
  fmap f (IslT g) = IslT $ \ctx -> fmap f (g ctx)

instance Monad m => Applicative (IslT m) where
  pure a = IslT $ \_ -> pure a
  IslT f <*> IslT a = IslT $ \ctx -> do
    f' <- f ctx
    a' <- a ctx
    return (f' a')

instance Monad m => Monad (IslT m) where
  IslT m >>= k = IslT $ \ctx -> do
    a <- m ctx
    unIslT (k a) ctx

instance MonadTrans IslT where
  lift m = IslT $ \_ -> m

instance MonadIO m => MonadIO (IslT m) where
  liftIO = lift . liftIO

-- | Run an ISL computation in a transformer stack. Allocates an ISL context,
-- runs the computation, forces the result (via 'NFData'), then frees the
-- context. The 'Ur' return type ensures no linear ISL objects escape.
runIslT :: (MonadIO m, NFData a) => IslT m (Ur a) -> m a
runIslT (IslT f) = do
  ctxPtr <- liftIO c_ctx_alloc
  Ur a <- f (Ctx ctxPtr)
  liftIO $ evaluate (rnf a)
  liftIO $ c_ctx_free ctxPtr
  return a

-- | Run an ISL computation in pure context. Convenience wrapper around
-- 'runIslT' specialized to 'IO'.
runIsl :: NFData a => Isl (Ur a) -> a
runIsl m = unsafePerformIO $ runIslT m

-- | Retrieve the ISL context. Used by generated code.
getCtx :: Monad m => IslT m Ctx
getCtx = IslT $ \ctx -> return ctx

-- | Lift a raw IO action (that needs the context) into 'IslT'.
-- Used by generated code for FFI calls.
unsafeIslFromIO :: MonadIO m => (Ctx -> IO a) -> IslT m a
unsafeIslFromIO f = IslT $ \ctx -> liftIO (f ctx)

-- | Free an ISL object within the IslT monad. Unlike 'consume' (which uses
-- unsafePerformIO and can be deferred by lazy evaluation), 'freeM' is
-- sequenced by the monad — the free happens in-order before subsequent actions.
freeM :: (MonadIO m, Consumable a) => a -> IslT m ()
freeM x = IslT $ \_ -> liftIO $ evaluate (consume x)
