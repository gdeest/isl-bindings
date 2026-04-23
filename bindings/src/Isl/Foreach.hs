{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal callback infrastructure for ISL foreach iteration.
-- Public foreach functions are re-exported from the relevant type modules
-- (e.g. 'Isl.BasicSet.foreachConstraint').
--
-- The combinators run inside 'Isl' ('IslT IO') and take a rank-2 callback
-- @forall s. ref s -> Isl (Ur r)@. The skolem @s@ prevents the
-- borrowed ref from escaping into the result; 'NFData' on @r@ forces
-- deep evaluation at the callback boundary so no lazy thunk retains
-- the ref past the point where ISL frees it.
--
-- (Specialised to 'IO' because the ISL C callback runs in 'IO' and
-- 'bracket' cannot unlift an arbitrary @m@. All current callers are in
-- 'IO' context.)
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
    -- * Generic foreach collection (region-tagged, auto-free)
  , foreachCollect
  , foreachCollect2
  ) where

import Data.IORef
import Data.Kind (Type)
import Foreign.C.Types
import Foreign.Ptr
import Control.DeepSeq (NFData, rnf)
import Control.Exception (bracket, evaluate)
import Unsafe.Coerce (unsafeCoerce)

import Isl.Types.Raw
import Isl.Types.Internal (Consumable(..), Borrow(..))
import Isl.Monad.Internal (Isl, IslT(..), Ur(..))

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
-- Generic foreach → list collection (region-tagged, auto-free)
------------------------------------------------------------------------

-- | Generic foreach collection pattern.
--
-- Each iteration receives the ISL element as an owned value; we hand the
-- user's callback a borrowed ref (scoped to a fresh skolem @s@ via
-- 'borrow'), deep-force the result, then consume the element. The rank-2
-- callback prevents the ref from appearing in the collected list; the
-- 'NFData' + 'rnf' pair prevents lazy thunks in @r@ from outliving the
-- freed element.
foreachCollect
  :: forall a (aRef :: Type -> Type) r
   . (NFData r, Consumable a, Borrow a aRef)
  => (forall x. (a -> Ptr () -> IO CInt) -> IO (FunPtr (a -> Ptr () -> IO CInt)))
     -- ^ wrapper function (from @foreign import ccall "wrapper"@)
  -> (FunPtr (a -> Ptr () -> IO CInt) -> IO CInt)
     -- ^ the C foreach call, partially applied with the ISL object
  -> (forall s. aRef s -> Isl (Ur r))
     -- ^ process each element; ref is region-scoped and cannot escape
  -> Isl (Ur [r])
foreachCollect = unsafeCoerce go
  where
    go :: (forall x. (a -> Ptr () -> IO CInt) -> IO (FunPtr (a -> Ptr () -> IO CInt)))
       -> (FunPtr (a -> Ptr () -> IO CInt) -> IO CInt)
       -> (aRef () -> Isl (Ur r))
       -> Isl (Ur [r])
    go mkWrapper doForeach process = IslT $ \ctx -> do
      ref <- newIORef []
      bracket
        (mkWrapper $ \element _user -> do
          -- borrow's rank-2 callback accepts `a` = `Isl (Ur r)` here
          -- since the result type doesn't mention the region skolem.
          let !(action, element') = borrow element (unsafeCoerce process)
          Ur !result <- unIslT action ctx
          evaluate (rnf result)
          consume element'
          modifyIORef' ref (result :)
          return 0)  -- isl_stat_ok
        freeHaskellFunPtr
        (\cb -> do
          _ <- doForeach cb
          Ur . reverse <$> readIORef ref)

-- | Like 'foreachCollect' but for two-argument callbacks (e.g. piecewise
-- iteration). Both refs share a single region @s@ because both live
-- inside the same callback invocation.
--
-- The 'Borrow' class only supports single-ref rank-2 scoping. For the
-- two-ref case we build the refs via 'unsafeCoerce' from the owned
-- values' pointer representations, preserving the representational
-- equivalence between @Foo@ and @FooRef s@ while letting GHC enforce
-- the shared-region skolem at the @process@ call site.
foreachCollect2
  :: forall a (aRef :: Type -> Type) b (bRef :: Type -> Type) r
   . (NFData r, Consumable a, Borrow a aRef, Consumable b, Borrow b bRef)
  => (forall x. (a -> b -> Ptr () -> IO CInt) -> IO (FunPtr (a -> b -> Ptr () -> IO CInt)))
     -- ^ wrapper function
  -> (FunPtr (a -> b -> Ptr () -> IO CInt) -> IO CInt)
     -- ^ the C foreach call, partially applied with the ISL object
  -> (forall s. aRef s -> bRef s -> Isl (Ur r))
     -- ^ process each pair; refs are region-scoped and cannot escape
  -> Isl (Ur [r])
foreachCollect2 = unsafeCoerce go
  where
    go :: (forall x. (a -> b -> Ptr () -> IO CInt) -> IO (FunPtr (a -> b -> Ptr () -> IO CInt)))
       -> (FunPtr (a -> b -> Ptr () -> IO CInt) -> IO CInt)
       -> (aRef () -> bRef () -> Isl (Ur r))
       -> Isl (Ur [r])
    go mkWrapper doForeach process = IslT $ \ctx -> do
      ref <- newIORef []
      bracket
        (mkWrapper $ \x y _user -> do
          -- Cast owned values to refs sharing a single skolem.
          -- Safe: newtype wrappers over the same Ptr representation.
          let !xRef = unsafeCoerce x :: aRef ()
              !yRef = unsafeCoerce y :: bRef ()
          Ur !result <- unIslT (process xRef yRef) ctx
          evaluate (rnf result)
          consume x
          consume y
          modifyIORef' ref (result :)
          return 0)  -- isl_stat_ok
        freeHaskellFunPtr
        (\cb -> do
          _ <- doForeach cb
          Ur . reverse <$> readIORef ref)
