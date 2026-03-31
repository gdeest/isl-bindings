{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}

-- | Internal callback infrastructure for ISL foreach iteration.
-- Public foreach functions are re-exported from the relevant type modules
-- (e.g. 'Isl.BasicSet.foreachConstraint').
module Isl.Foreach
  ( -- * Callback types
    RawCallback
  , RawCallback2
    -- * Callback wrappers (single-argument)
  , mkConstraintCb
  , mkBasicSetCb
  , mkSetCb
  , mkBasicMapCb
  , mkMapCb
    -- * Callback wrappers (two-argument, for piecewise iteration)
  , mkPwAffPieceCb
  , mkPwMultiAffPieceCb
    -- * Generic foreach collection
  , foreachCollect
  , foreachCollect2
  ) where

import Data.IORef
import Foreign.C.Types
import Foreign.Ptr
import Control.Exception (bracket)

import Isl.Types

------------------------------------------------------------------------
-- Callback types and wrappers
------------------------------------------------------------------------

type RawCallback a = a -> Ptr () -> IO CInt
type RawCallback2 a b = a -> b -> Ptr () -> IO CInt

foreign import ccall "wrapper"
  mkConstraintCb :: RawCallback Constraint -> IO (FunPtr (RawCallback Constraint))

foreign import ccall "wrapper"
  mkBasicSetCb :: RawCallback BasicSet -> IO (FunPtr (RawCallback BasicSet))

foreign import ccall "wrapper"
  mkSetCb :: RawCallback Set -> IO (FunPtr (RawCallback Set))

foreign import ccall "wrapper"
  mkBasicMapCb :: RawCallback BasicMap -> IO (FunPtr (RawCallback BasicMap))

foreign import ccall "wrapper"
  mkMapCb :: RawCallback Map -> IO (FunPtr (RawCallback Map))

foreign import ccall "wrapper"
  mkPwAffPieceCb :: RawCallback2 Set Aff -> IO (FunPtr (RawCallback2 Set Aff))

foreign import ccall "wrapper"
  mkPwMultiAffPieceCb :: RawCallback2 Set MultiAff -> IO (FunPtr (RawCallback2 Set MultiAff))

------------------------------------------------------------------------
-- Generic foreach → list collection
------------------------------------------------------------------------

-- | Generic foreach collection pattern. Creates a FunPtr callback that
-- applies @process@ to each element, accumulating results in an IORef.
-- The FunPtr is freed even if an exception occurs.
foreachCollect
  :: (forall b. (a -> Ptr () -> IO CInt) -> IO (FunPtr (a -> Ptr () -> IO CInt)))
     -- ^ wrapper function (from @foreign import ccall "wrapper"@)
  -> (FunPtr (a -> Ptr () -> IO CInt) -> IO CInt)
     -- ^ the C foreach call, partially applied with the ISL object
  -> (a -> IO r)
     -- ^ process each element (element is __isl_take — must be freed by caller)
  -> IO [r]
foreachCollect mkWrapper doForeach process = do
  ref <- newIORef []
  bracket
    (mkWrapper $ \element _user -> do
      result <- process element
      modifyIORef' ref (result :)
      return 0)  -- isl_stat_ok
    freeHaskellFunPtr
    (\cb -> do
      _ <- doForeach cb
      reverse <$> readIORef ref)

-- | Like 'foreachCollect' but for two-argument callbacks (e.g. piecewise iteration).
foreachCollect2
  :: (forall x. (a -> b -> Ptr () -> IO CInt) -> IO (FunPtr (a -> b -> Ptr () -> IO CInt)))
     -- ^ wrapper function
  -> (FunPtr (a -> b -> Ptr () -> IO CInt) -> IO CInt)
     -- ^ the C foreach call, partially applied with the ISL object
  -> (a -> b -> IO r)
     -- ^ process each pair (both are __isl_take)
  -> IO [r]
foreachCollect2 mkWrapper doForeach process = do
  ref <- newIORef []
  bracket
    (mkWrapper $ \x y _user -> do
      result <- process x y
      modifyIORef' ref (result :)
      return 0)  -- isl_stat_ok
    freeHaskellFunPtr
    (\cb -> do
      _ <- doForeach cb
      reverse <$> readIORef ref)
