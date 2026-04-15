{-# LANGUAGE BangPatterns #-}
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
    -- * Generic foreach collection (auto-borrow, auto-free)
  , foreachCollect
  , foreachCollect2
  ) where

import Data.IORef
import Foreign.C.Types
import Foreign.Ptr
import Control.Exception (bracket, evaluate)

import Isl.Types
import Isl.Types.Internal (Consumable(..), Borrow(..))

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
-- Generic foreach → list collection (auto-borrow, auto-free)
------------------------------------------------------------------------

-- | Generic foreach collection pattern with automatic resource management.
-- The callback receives a borrowed Ref; the owned element is automatically
-- freed after the callback returns. Creates a FunPtr callback, runs the
-- C foreach, then frees the FunPtr.
foreachCollect
  :: (Consumable a, Borrow a aRef)
  => (forall b. (a -> Ptr () -> IO CInt) -> IO (FunPtr (a -> Ptr () -> IO CInt)))
     -- ^ wrapper function (from @foreign import ccall "wrapper"@)
  -> (FunPtr (a -> Ptr () -> IO CInt) -> IO CInt)
     -- ^ the C foreach call, partially applied with the ISL object
  -> (aRef -> IO r)
     -- ^ process each element via borrowed Ref (element is auto-freed)
  -> IO [r]
foreachCollect mkWrapper doForeach process = do
  ref <- newIORef []
  bracket
    (mkWrapper $ \element _user -> do
      let !(eRef, element') = borrow element (\r -> r)
      !result <- process eRef
      _ <- evaluate (consume element')
      modifyIORef' ref (result :)
      return 0)  -- isl_stat_ok
    freeHaskellFunPtr
    (\cb -> do
      _ <- doForeach cb
      reverse <$> readIORef ref)

-- | Like 'foreachCollect' but for two-argument callbacks (e.g. piecewise iteration).
-- Both arguments are auto-borrowed and auto-freed.
foreachCollect2
  :: (Consumable a, Borrow a aRef, Consumable b, Borrow b bRef)
  => (forall x. (a -> b -> Ptr () -> IO CInt) -> IO (FunPtr (a -> b -> Ptr () -> IO CInt)))
     -- ^ wrapper function
  -> (FunPtr (a -> b -> Ptr () -> IO CInt) -> IO CInt)
     -- ^ the C foreach call, partially applied with the ISL object
  -> (aRef -> bRef -> IO r)
     -- ^ process each pair via borrowed Refs (elements are auto-freed)
  -> IO [r]
foreachCollect2 mkWrapper doForeach process = do
  ref <- newIORef []
  bracket
    (mkWrapper $ \x y _user -> do
      let !(xRef, x') = borrow x (\r -> r)
          !(yRef, y') = borrow y (\r -> r)
      !result <- process xRef yRef
      _ <- evaluate (consume x')
      _ <- evaluate (consume y')
      modifyIORef' ref (result :)
      return 0)  -- isl_stat_ok
    freeHaskellFunPtr
    (\cb -> do
      _ <- doForeach cb
      reverse <$> readIORef ref)
