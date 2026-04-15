{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}

-- | Internal module — exports the 'IslT' constructor and internal helpers.
-- Not importable from outside the package (listed in other-modules).
-- External code should use 'Isl.Monad' (opaque IslT) and 'Isl.Linear'.
module Isl.Monad.Internal
  ( IslT(..)
  , Isl
  , Ur(..)
  , Both(..)
  , runIslT
  , runIsl
  , withIslCtx
  , getCtx
  , unsafeIslFromIO
  , checkIslError
  ) where

import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.String (peekCString)
import Foreign.Ptr (Ptr, nullPtr)
import System.IO.Unsafe (unsafePerformIO)

import Isl.Types

-- | Unrestricted wrapper. Wrapping a value in 'Ur' signals that it contains
-- no linear ISL resources and may be freely shared.
data Ur a where
  Ur :: a -> Ur a

-- | Linear pair. Unlike standard '(,)', pattern matching a linear 'Both'
-- gives both components with multiplicity One — essential for returning
-- an ISL object alongside a query result from 'Isl.Linear.query'.
data Both a b where
  Both :: a %1 -> b %1 -> Both a b

-- | The ISL monad transformer. Carries an ISL context through a monadic
-- computation. Has no standard 'Monad' instance — use @Isl.do@ from
-- "Isl.Linear" to sequence operations with linear type enforcement.
newtype IslT m a = IslT { unIslT :: Ctx -> m a }

-- | Concrete ISL monad — 'IslT' specialized to 'IO'.
type Isl = IslT IO

-- | Run an ISL computation in a transformer stack.
runIslT :: (MonadIO m, NFData a) => IslT m (Ur a) -> m a
runIslT (IslT f) = do
  ctxPtr <- liftIO c_ctx_alloc
  Ur a <- f (Ctx ctxPtr)
  liftIO $ evaluate (rnf a)
  liftIO $ do
    throwIslError "runIslT" ctxPtr
    c_ctx_free ctxPtr
  return a

-- | Run an ISL computation in pure context.
runIsl :: NFData a => Isl (Ur a) -> a
runIsl m = unsafePerformIO $ runIslT m

-- | Run against an existing context (not freed by this call).
withIslCtx :: (MonadIO m, NFData a) => Ctx -> IslT m (Ur a) -> m a
withIslCtx ctx@(Ctx ctxPtr) (IslT f) = do
  Ur a <- f ctx
  liftIO $ evaluate (rnf a)
  liftIO $ throwIslError "withIslCtx" ctxPtr
  return a

-- | Retrieve the ISL context. Used by generated code.
getCtx :: Monad m => IslT m Ctx
getCtx = IslT $ \ctx -> return ctx

-- | Lift a raw IO action into 'IslT'. Used by generated code.
unsafeIslFromIO :: MonadIO m => (Ctx -> IO a) -> IslT m a
unsafeIslFromIO f = IslT $ \ctx -> liftIO (f ctx)

throwIslError :: String -> Ptr Ctx -> IO ()
throwIslError label ctxPtr = do
  err <- c_ctx_last_error ctxPtr
  when (err /= 0) $ do
    msgPtr  <- c_ctx_last_error_msg ctxPtr
    filePtr <- c_ctx_last_error_file ctxPtr
    line    <- c_ctx_last_error_line ctxPtr
    msg  <- if msgPtr  /= nullPtr then peekCString msgPtr  else pure "(no message)"
    file <- if filePtr /= nullPtr then peekCString filePtr else pure "(unknown file)"
    error $ "ISL error at [" ++ label ++ "] (code " ++ show err ++ "): " ++ msg
         ++ " [" ++ file ++ ":" ++ show line ++ "]"

-- | Check for ISL errors and crash with details. Resets error state.
checkIslError :: MonadIO m => String -> IslT m ()
checkIslError label = IslT $ \(Ctx ctxPtr) -> liftIO $ do
  throwIslError label ctxPtr
  c_ctx_reset_error ctxPtr
