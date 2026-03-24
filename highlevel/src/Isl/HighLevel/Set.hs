{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Isl.HighLevel.Set where

import Control.Monad.IO.Class (MonadIO)
import Data.Proxy
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.BasicSet (BasicSet(..), extractSetConstraint)
import Isl.HighLevel.Constraints (Conjunction(..), SetIx)
import Isl.HighLevel.Params (KnownSymbols(..), Length, Union)
import Isl.HighLevel.Pure (PConjunction(..), PDisjunction(..))

import qualified Isl.Types as Isl
import Isl.Types (Borrow(..), Dupable(..), Consumable(..))
import Isl.Instances ()
import Isl.Monad (IslT(..), Ur(..), unsafeIslFromIO, withCtx, freeM)
import qualified Isl.Foreach as Foreach
import qualified Isl.Set.AutoGen as S

-- | Owned, parameter- and dimension-indexed Set.
-- Linear — must be consumed exactly once.
newtype Set (ps :: [Symbol]) (nDims :: Nat) = Set Isl.Set

-- | Borrowed reference to a Set. Unrestricted — safe for queries.
newtype SetRef (ps :: [Symbol]) (nDims :: Nat) = SetRef Isl.SetRef

instance Consumable (Set ps n) where
  consume = unsafeCoerce $ \(Set s) -> consume s

instance Dupable (Set ps n) where
  dup = unsafeCoerce $ \(Set s) ->
    let (a, b) = dup s in (Set a, Set b)

instance Borrow (Set ps n) (SetRef ps n) where
  borrow = unsafeCoerce $ \(Set s) f ->
    let (result, s') = borrow s (\ref -> f (SetRef ref))
    in (result, Set s')

-- Construction

fromBasicSet :: forall m ps n. MonadIO m => BasicSet ps n %1 -> IslT m (Set ps n)
fromBasicSet = unsafeCoerce go
  where
    go :: BasicSet ps n -> IslT m (Set ps n)
    go (BasicSet bs) = Set <$> withCtx (S.fromBasicSet bs)

fromString :: forall m ps n. MonadIO m => String -> IslT m (Set ps n)
fromString str = Set <$> withCtx (S.readFromStr str)

-- Operations (consuming)
-- Binary operations use Union to merge parameter spaces, matching ISL's behavior.

union :: forall m ps1 ps2 n. MonadIO m
  => Set ps1 n %1 -> Set ps2 n %1 -> IslT m (Set (Union ps1 ps2) n)
union = unsafeCoerce go
  where
    go :: Set ps1 n -> Set ps2 n -> IslT m (Set (Union ps1 ps2) n)
    go (Set s1) (Set s2) = Set <$> withCtx (S.union s1 s2)

intersect :: forall m ps1 ps2 n. MonadIO m
  => Set ps1 n %1 -> Set ps2 n %1 -> IslT m (Set (Union ps1 ps2) n)
intersect = unsafeCoerce go
  where
    go :: Set ps1 n -> Set ps2 n -> IslT m (Set (Union ps1 ps2) n)
    go (Set s1) (Set s2) = Set <$> withCtx (S.intersect s1 s2)

subtract :: forall m ps1 ps2 n. MonadIO m
  => Set ps1 n %1 -> Set ps2 n %1 -> IslT m (Set (Union ps1 ps2) n)
subtract = unsafeCoerce go
  where
    go :: Set ps1 n -> Set ps2 n -> IslT m (Set (Union ps1 ps2) n)
    go (Set s1) (Set s2) = Set <$> withCtx (S.subtract s1 s2)

complement :: forall m ps n. MonadIO m => Set ps n %1 -> IslT m (Set ps n)
complement = unsafeCoerce go
  where
    go :: Set ps n -> IslT m (Set ps n)
    go (Set s) = Set <$> withCtx (S.complement s)

coalesce :: forall m ps n. MonadIO m => Set ps n %1 -> IslT m (Set ps n)
coalesce = unsafeCoerce go
  where
    go :: Set ps n -> IslT m (Set ps n)
    go (Set s) = Set <$> withCtx (S.coalesce s)

-- Predicates (borrowing — isl_keep, safe to return objects after query)

isEmpty :: forall m ps n. Monad m => Set ps n %1 -> IslT m (Ur Bool, Set ps n)
isEmpty = unsafeCoerce go
  where
    go :: Set ps n -> IslT m (Ur Bool, Set ps n)
    go (Set s) = do
      r <- withCtx (S.isEmpty s)
      return (Ur r, Set s)

isEqual :: forall m ps n. Monad m => Set ps n %1 -> Set ps n %1 -> IslT m (Ur Bool, Set ps n, Set ps n)
isEqual = unsafeCoerce go
  where
    go :: Set ps n -> Set ps n -> IslT m (Ur Bool, Set ps n, Set ps n)
    go (Set s1) (Set s2) = do
      r <- withCtx (S.isEqual s1 s2)
      return (Ur r, Set s1, Set s2)

isSubset :: forall m ps n. Monad m => Set ps n %1 -> Set ps n %1 -> IslT m (Ur Bool, Set ps n, Set ps n)
isSubset = unsafeCoerce go
  where
    go :: Set ps n -> Set ps n -> IslT m (Ur Bool, Set ps n, Set ps n)
    go (Set s1) (Set s2) = do
      r <- withCtx (S.isSubset s1 s2)
      return (Ur r, Set s1, Set s2)

-- Conversion

toUnionSet :: forall m ps n. MonadIO m => Set ps n %1 -> IslT m Isl.UnionSet
toUnionSet = unsafeCoerce go
  where
    go :: Set ps n -> IslT m Isl.UnionSet
    go (Set s) = withCtx (S.toUnionSet s)

-- Queries

setToString :: SetRef ps n -> String
setToString (SetRef sRef) = unsafePerformIO $ Foreach.setToStr sRef

borrowSet :: forall m ps n a. Monad m => Set ps n %1 -> (SetRef ps n -> a) -> IslT m (Ur a, Set ps n)
borrowSet = unsafeCoerce go
  where
    go :: Set ps n -> (SetRef ps n -> a) -> IslT m (Ur a, Set ps n)
    go (Set s) f =
      let !(result, s') = borrow s (\ref -> f (SetRef ref))
      in IslT $ \_ -> return (Ur result, Set s')

-- Decomposition

decomposeSet :: forall m ps n. (MonadIO m, KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => Set ps n %1 -> IslT m (Ur (PDisjunction ps n), Set ps n)
decomposeSet = unsafeCoerce go
  where
    nParams = fromIntegral $ natVal (Proxy @(Length ps))
    nDims = fromIntegral $ natVal (Proxy @n)

    go :: Set ps n -> IslT m (Ur (PDisjunction ps n), Set ps n)
    go (Set rawSet) = do
      let !(ref, rawSet') = borrow rawSet (\r -> r)
      conjunctions <- unsafeIslFromIO $ \_ ->
        Foreach.setForeachBasicSet ref $ \bs -> do
          let !(bsRef, _) = borrow bs (\r -> r)
          constraints <- Foreach.basicSetForeachConstraint bsRef $ \c -> do
            result <- extractSetConstraint nParams nDims c
            Foreach.constraintFree c
            return result
          Foreach.basicSetFree bs
          return (PConjunction (Conjunction constraints))
      return (Ur (PDisjunction conjunctions), Set rawSet')

-- Resource management

freeSet :: forall m ps n. MonadIO m => Set ps n %1 -> IslT m ()
freeSet = unsafeCoerce go
  where
    go :: Set ps n -> IslT m ()
    go (Set s) = freeM s
