{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Isl.HighLevel.Set where

import Control.Monad.IO.Class (MonadIO)
import Data.Proxy
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.BasicSet (BasicSet(..), extractSetConstraint)
import Isl.HighLevel.Constraints (Conjunction(..))
import Isl.HighLevel.Pure (PConjunction(..), PDisjunction(..))

import qualified Isl.Types as Isl
import Isl.Types (Borrow(..), Dupable(..), Consumable(..))
import Isl.Instances ()
import Isl.Monad (IslT(..), Ur(..), unsafeIslFromIO, withCtx, freeM)
import qualified Isl.Foreach as Foreach
import qualified Isl.Set.AutoGen as S

-- | Owned, dimension-indexed Set. Linear — must be consumed exactly once.
newtype Set (nDims :: Nat) = Set Isl.Set

-- | Borrowed reference to a Set. Unrestricted — safe for queries.
newtype SetRef (nDims :: Nat) = SetRef Isl.SetRef

instance Consumable (Set n) where
  consume = unsafeCoerce $ \(Set s) -> consume s

instance Dupable (Set n) where
  dup = unsafeCoerce $ \(Set s) ->
    let (a, b) = dup s in (Set a, Set b)

instance Borrow (Set n) (SetRef n) where
  borrow = unsafeCoerce $ \(Set s) f ->
    let (result, s') = borrow s (\ref -> f (SetRef ref))
    in (result, Set s')

-- Construction

fromBasicSet :: forall m n. MonadIO m => BasicSet n %1 -> IslT m (Set n)
fromBasicSet = unsafeCoerce go
  where
    go :: BasicSet n -> IslT m (Set n)
    go (BasicSet bs) = Set <$> withCtx (S.fromBasicSet bs)

fromString :: forall m n. MonadIO m => String -> IslT m (Set n)
fromString str = Set <$> withCtx (S.readFromStr str)

-- Operations (consuming)

union :: forall m n. MonadIO m => Set n %1 -> Set n %1 -> IslT m (Set n)
union = unsafeCoerce go
  where
    go :: Set n -> Set n -> IslT m (Set n)
    go (Set s1) (Set s2) = Set <$> withCtx (S.union s1 s2)

intersect :: forall m n. MonadIO m => Set n %1 -> Set n %1 -> IslT m (Set n)
intersect = unsafeCoerce go
  where
    go :: Set n -> Set n -> IslT m (Set n)
    go (Set s1) (Set s2) = Set <$> withCtx (S.intersect s1 s2)

subtract :: forall m n. MonadIO m => Set n %1 -> Set n %1 -> IslT m (Set n)
subtract = unsafeCoerce go
  where
    go :: Set n -> Set n -> IslT m (Set n)
    go (Set s1) (Set s2) = Set <$> withCtx (S.subtract s1 s2)

complement :: forall m n. MonadIO m => Set n %1 -> IslT m (Set n)
complement = unsafeCoerce go
  where
    go :: Set n -> IslT m (Set n)
    go (Set s) = Set <$> withCtx (S.complement s)

coalesce :: forall m n. MonadIO m => Set n %1 -> IslT m (Set n)
coalesce = unsafeCoerce go
  where
    go :: Set n -> IslT m (Set n)
    go (Set s) = Set <$> withCtx (S.coalesce s)

-- Predicates (borrowing — isl_keep, safe to return objects after query)

isEmpty :: forall m n. Monad m => Set n %1 -> IslT m (Ur Bool, Set n)
isEmpty = unsafeCoerce go
  where
    go :: Set n -> IslT m (Ur Bool, Set n)
    go (Set s) = do
      r <- withCtx (S.isEmpty s)
      return (Ur r, Set s)

isEqual :: forall m n. Monad m => Set n %1 -> Set n %1 -> IslT m (Ur Bool, Set n, Set n)
isEqual = unsafeCoerce go
  where
    go :: Set n -> Set n -> IslT m (Ur Bool, Set n, Set n)
    go (Set s1) (Set s2) = do
      r <- withCtx (S.isEqual s1 s2)
      return (Ur r, Set s1, Set s2)

isSubset :: forall m n. Monad m => Set n %1 -> Set n %1 -> IslT m (Ur Bool, Set n, Set n)
isSubset = unsafeCoerce go
  where
    go :: Set n -> Set n -> IslT m (Ur Bool, Set n, Set n)
    go (Set s1) (Set s2) = do
      r <- withCtx (S.isSubset s1 s2)
      return (Ur r, Set s1, Set s2)

-- Conversion

toUnionSet :: forall m n. MonadIO m => Set n %1 -> IslT m Isl.UnionSet
toUnionSet = unsafeCoerce go
  where
    go :: Set n -> IslT m Isl.UnionSet
    go (Set s) = withCtx (S.toUnionSet s)

-- Queries

setToString :: SetRef n -> String
setToString (SetRef sRef) = unsafePerformIO $ Foreach.setToStr sRef

borrowSet :: forall m n a. Monad m => Set n %1 -> (SetRef n -> a) -> IslT m (Ur a, Set n)
borrowSet = unsafeCoerce go
  where
    go :: Set n -> (SetRef n -> a) -> IslT m (Ur a, Set n)
    go (Set s) f =
      let !(result, s') = borrow s (\ref -> f (SetRef ref))
      in IslT $ \_ -> return (Ur result, Set s')

-- Decomposition

decomposeSet :: forall m n. (MonadIO m, KnownNat n)
  => Set n %1 -> IslT m (Ur (PDisjunction n), Set n)
decomposeSet = unsafeCoerce go
  where
    nDims = fromIntegral $ natVal (Proxy @n)

    go :: Set n -> IslT m (Ur (PDisjunction n), Set n)
    go (Set rawSet) = do
      let !(ref, rawSet') = borrow rawSet (\r -> r)
      conjunctions <- unsafeIslFromIO $ \_ ->
        Foreach.setForeachBasicSet ref $ \bs -> do
          let !(bsRef, _) = borrow bs (\r -> r)
          constraints <- Foreach.basicSetForeachConstraint bsRef $ \c -> do
            result <- extractSetConstraint nDims c
            Foreach.constraintFree c
            return result
          Foreach.basicSetFree bs
          return (PConjunction (Conjunction constraints))
      return (Ur (PDisjunction conjunctions), Set rawSet')

-- Resource management

freeSet :: forall m n. MonadIO m => Set n %1 -> IslT m ()
freeSet = unsafeCoerce go
  where
    go :: Set n -> IslT m ()
    go (Set s) = freeM s
