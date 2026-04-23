{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Linear do-notation for the 'IslT' monad via @QualifiedDo@.
--
-- This is the mandatory interface for composing ISL operations.
-- 'IslT' has no standard 'Monad' instance — the only way to sequence
-- ISL actions is through @Isl.do@ (QualifiedDo).
--
-- Usage:
--
-- @
-- {-\# LANGUAGE QualifiedDo \#-}
-- import qualified Isl.Linear as Isl
-- import Isl.Linear (query, query_, freeM)
--
-- example = runIsl $ Isl.do
--   x <- someAction          -- x has multiplicity One
--   Ur v <- query_ x getSomething  -- query and free in one shot
--   Isl.pure (Ur v)
-- @
--
-- Values bound by @<-@ have multiplicity One — GHC enforces that every
-- ISL object is consumed exactly once (freed, passed to an isl_take
-- function, or duplicated via 'dup').
--
-- The @query*@ combinators are rank-2 in a region skolem @s@: the ref
-- cannot escape into the callback's result because the result type is
-- introduced outside the @forall s.@. Internally the helpers fix @s ~ ()@
-- (via 'unsafeCoerce') so the wrapper functions can be given in plain
-- non-polymorphic form; callers still see the rank-2 external type.
module Isl.Linear
  ( -- * QualifiedDo operators
    (>>=)
  , (>>)
  , pure
  , fail
    -- * Querying owned objects
  , query
  , query_
  , queryM
  , queryM_
    -- * Resource management
  , freeM
  , dupM
  , urWrap
    -- * Re-exports
  , Both(..)
    -- * Traversal
  , mapM
  , foldM
  ) where

import Prelude (($), String, Monad, (.), map, IO)
import qualified Prelude
import qualified Control.Monad.Fail as Fail
import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Kind (Type)
import Isl.Monad.Internal (IslT(..), Ur(..), Both(..))
import Isl.Types.Internal (Borrow(..), Consumable(..), Dupable(dup))
import Unsafe.Coerce (unsafeCoerce)

-- | Linear bind. With @QualifiedDo@, @x <- action@ gives @x@ multiplicity One.
(>>=) :: forall m a b. Monad m => IslT m a %1 -> (a %1 -> IslT m b) %1 -> IslT m b
(>>=) = unsafeCoerce go
  where
    go :: IslT m a -> (a -> IslT m b) -> IslT m b
    go (IslT m) k = IslT $ \ctx -> do
      a <- m ctx
      unIslT (k a) ctx

-- | Linear sequence.
(>>) :: forall m b. Monad m => IslT m () %1 -> IslT m b %1 -> IslT m b
m >> k = m >>= \() -> k

-- | Linear pure.
pure :: forall m a. Monad m => a %1 -> IslT m a
pure = unsafeCoerce go
  where
    go :: a -> IslT m a
    go a = IslT $ \_ -> Prelude.return a

-- | Fail (for failable pattern matches in QualifiedDo).
fail :: Fail.MonadFail m => String -> IslT m a
fail s = IslT $ \_ -> Fail.fail s

-- | Borrow an owned value, apply a pure query, and return the result
-- wrapped in 'Ur' (unrestricted) alongside the still-owned value.
--
-- The callback is rank-2 in a region skolem @s@: the ref cannot escape
-- into the result @a@ because @a@ is introduced before @forall s.@.
query
  :: forall m owned (ref :: Type -> Type) a
   . (Monad m, Borrow owned ref)
  => owned %1
  -> (forall s. ref s -> a)
  -> IslT m (Both (Ur a) owned)
query = unsafeCoerce go
  where
    go :: owned -> (forall s. ref s -> a) -> IslT m (Both (Ur a) owned)
    go owned f =
      let !(result, owned') = borrow owned f
      in IslT $ \_ -> Prelude.return (Both (Ur result) owned')

-- | Query an owned value and free it in one shot.
query_
  :: forall m owned (ref :: Type -> Type) a
   . (MonadIO m, Borrow owned ref, Consumable owned)
  => owned %1
  -> (forall s. ref s -> a)
  -> IslT m (Ur a)
query_ = unsafeCoerce go
  where
    go :: (Consumable owned) => owned -> (forall s. ref s -> a) -> IslT m (Ur a)
    go owned f =
      let !(result, owned') = borrow owned f
      in IslT $ \_ -> do
        liftIO (consume owned')
        Prelude.return (Ur result)

-- | Monadic borrow. Borrow an owned value for the duration of a monadic
-- action (e.g. calling foreach on a borrowed object). The owned value is
-- returned alongside the action result.
queryM
  :: forall m owned (ref :: Type -> Type) a
   . (MonadIO m, Borrow owned ref)
  => owned %1
  -> (forall s. ref s -> IslT m (Ur a)) %1
  -> IslT m (Both (Ur a) owned)
queryM = unsafeCoerce go
  where
    go :: owned -> (forall s. ref s -> IslT m (Ur a)) -> IslT m (Both (Ur a) owned)
    go owned f =
      let !(action, owned') = borrow owned f
      in IslT $ \ctx -> do
        result <- unIslT action ctx
        Prelude.return (Both result owned')

-- | Like 'queryM' but auto-frees the owned object after the callback.
queryM_
  :: forall m owned (ref :: Type -> Type) a
   . (MonadIO m, Borrow owned ref, Consumable owned)
  => owned %1
  -> (forall s. ref s -> IslT m (Ur a)) %1
  -> IslT m (Ur a)
queryM_ = unsafeCoerce go
  where
    go :: (Consumable owned)
       => owned -> (forall s. ref s -> IslT m (Ur a)) -> IslT m (Ur a)
    go owned f =
      let !(action, owned') = borrow owned f
      in IslT $ \ctx -> do
        Ur !result <- unIslT action ctx
        liftIO (consume owned')
        Prelude.return (Ur result)

-- | Free an ISL object within the 'IslT' monad. 'consume' now returns @IO ()@
-- so the free is sequenced by the monad; 'freeM' is the linear-arrow wrapper.
freeM :: forall m a. (MonadIO m, Consumable a) => a %1 -> IslT m ()
freeM = unsafeCoerce go
  where
    go :: Consumable a => a -> IslT m ()
    go x = IslT $ \_ -> liftIO (consume x)

-- | Duplicate an ISL object within the 'IslT' monad. 'dup' now returns
-- @IO (a, a)@ so the copy is sequenced by the monad; 'dupM' is the linear
-- entry point to use from @Isl.do@ blocks.
dupM :: forall m a. (MonadIO m, Dupable a) => a %1 -> IslT m (a, a)
dupM = unsafeCoerce go
  where
    go :: Dupable a => a -> IslT m (a, a)
    go x = IslT $ \_ -> liftIO (dup x)

-- | Map a function over a list, sequencing the IslT effects.
-- Returns @Ur@ so the result list can be used unrestrictedly in @Isl.do@.
-- Replacement for 'Prelude.mapM' which requires a 'Monad' instance.
mapM :: Monad m => (a -> IslT m b) -> [a] -> IslT m (Ur [b])
mapM _ [] = IslT $ \_ -> Prelude.return (Ur [])
mapM f (x:xs) = IslT $ \ctx -> do
  y  <- unIslT (f x) ctx
  Ur ys <- unIslT (mapM f xs) ctx
  Prelude.return (Ur (y : ys))

-- | Left fold over a list, sequencing IslT effects.
-- The accumulator function may take ownership (%1) of the accumulator.
-- Replacement for 'Control.Monad.foldM' which requires a 'Monad' instance.
foldM :: forall m b a. Monad m => (b %1 -> a -> IslT m b) -> b %1 -> [a] -> IslT m b
foldM = unsafeCoerce go where
  go :: (b -> a -> IslT m b) -> b -> [a] -> IslT m b
  go _ acc [] = IslT $ \_ -> Prelude.return acc
  go f acc (x:xs) = IslT $ \ctx -> do
    acc' <- unIslT (f acc x) ctx
    unIslT (go f acc' xs) ctx

-- | Wrap a linear value in 'Ur', erasing linearity. Use when a linear
-- ISL object must be placed in an unrestricted container (e.g. a list).
-- The caller takes responsibility for consuming the unwrapped value.
urWrap :: forall m a. Monad m => a %1 -> IslT m (Ur a)
urWrap = unsafeCoerce go where
  go :: a -> IslT m (Ur a)
  go x = IslT $ \_ -> Prelude.return (Ur x)
