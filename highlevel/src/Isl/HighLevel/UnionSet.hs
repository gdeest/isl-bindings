{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Isl.HighLevel.UnionSet where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (catMaybes)
import GHC.TypeLits (someNatVal, SomeNat(..), KnownNat, Symbol)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.BasicSet (extractSetConstraint)
import Isl.HighLevel.Constraints (Conjunction(..), SetIx)
import Isl.HighLevel.Pure
import Isl.HighLevel.Set (Set(..))

import qualified Isl.Types as Isl
import Isl.Types (Borrow(..), Dupable(..), Consumable(..))
import Isl.Instances ()
import Isl.Monad (IslT(..), Ur(..), unsafeIslFromIO, withCtx, freeM)
import qualified Isl.Foreach as Foreach
import qualified Isl.UnionSet.AutoGen as US

-- | Owned UnionSet. Not dimension-indexed because a union can contain
-- sets of different spaces.
newtype UnionSet = UnionSet Isl.UnionSet

-- | Borrowed reference to a UnionSet.
newtype UnionSetRef = UnionSetRef Isl.UnionSetRef

instance Consumable UnionSet where
  consume = unsafeCoerce $ \(UnionSet us) -> consume us

instance Dupable UnionSet where
  dup = unsafeCoerce $ \(UnionSet us) ->
    let (a, b) = dup us in (UnionSet a, UnionSet b)

instance Borrow UnionSet UnionSetRef where
  borrow = unsafeCoerce $ \(UnionSet us) f ->
    let (result, us') = borrow us (\ref -> f (UnionSetRef ref))
    in (result, UnionSet us')

-- Construction

fromSet :: forall m (ps :: [Symbol]) n. MonadIO m => Set ps n %1 -> IslT m UnionSet
fromSet = unsafeCoerce go
  where
    go :: Set ps n -> IslT m UnionSet
    go (Set s) = UnionSet <$> withCtx (US.fromSet s)

fromString :: forall m. MonadIO m => String -> IslT m UnionSet
fromString str = UnionSet <$> withCtx (US.readFromStr str)

-- Operations (consuming)

union :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m UnionSet
union = unsafeCoerce go
  where
    go :: UnionSet -> UnionSet -> IslT m UnionSet
    go (UnionSet us1) (UnionSet us2) = UnionSet <$> withCtx (US.union us1 us2)

intersect :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m UnionSet
intersect = unsafeCoerce go
  where
    go :: UnionSet -> UnionSet -> IslT m UnionSet
    go (UnionSet us1) (UnionSet us2) = UnionSet <$> withCtx (US.intersect us1 us2)

subtract :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m UnionSet
subtract = unsafeCoerce go
  where
    go :: UnionSet -> UnionSet -> IslT m UnionSet
    go (UnionSet us1) (UnionSet us2) = UnionSet <$> withCtx (US.subtract us1 us2)

coalesce :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
coalesce = unsafeCoerce go
  where
    go :: UnionSet -> IslT m UnionSet
    go (UnionSet us) = UnionSet <$> withCtx (US.coalesce us)

-- Predicates (borrowing)

isEmpty :: forall m. Monad m => UnionSet %1 -> IslT m (Ur Bool, UnionSet)
isEmpty = unsafeCoerce go
  where
    go :: UnionSet -> IslT m (Ur Bool, UnionSet)
    go (UnionSet us) = do
      r <- withCtx (US.isEmpty us)
      return (Ur r, UnionSet us)

isEqual :: forall m. Monad m => UnionSet %1 -> UnionSet %1 -> IslT m (Ur Bool, UnionSet, UnionSet)
isEqual = unsafeCoerce go
  where
    go :: UnionSet -> UnionSet -> IslT m (Ur Bool, UnionSet, UnionSet)
    go (UnionSet us1) (UnionSet us2) = do
      r <- withCtx (US.isEqual us1 us2)
      return (Ur r, UnionSet us1, UnionSet us2)

isSubset :: forall m. Monad m => UnionSet %1 -> UnionSet %1 -> IslT m (Ur Bool, UnionSet, UnionSet)
isSubset = unsafeCoerce go
  where
    go :: UnionSet -> UnionSet -> IslT m (Ur Bool, UnionSet, UnionSet)
    go (UnionSet us1) (UnionSet us2) = do
      r <- withCtx (US.isSubset us1 us2)
      return (Ur r, UnionSet us1, UnionSet us2)

-- Queries

usetToString :: UnionSetRef -> String
usetToString (UnionSetRef usRef) = unsafePerformIO $ Foreach.unionSetToStr usRef

borrowUS :: forall m a. Monad m => UnionSet %1 -> (UnionSetRef -> a) -> IslT m (Ur a, UnionSet)
borrowUS = unsafeCoerce go
  where
    go :: UnionSet -> (UnionSetRef -> a) -> IslT m (Ur a, UnionSet)
    go (UnionSet us) f =
      let !(result, us') = borrow us (\ref -> f (UnionSetRef ref))
      in IslT $ \_ -> return (Ur result, UnionSet us')

-- Decomposition
--
-- Note: Parameters are extracted at runtime and wrapped existentially
-- since the parameter names are not known statically for union types.
-- For now, we extract with 0 params (matching old behavior).
-- TODO: read parameter names from ISL space and wrap with KnownSymbols.

decomposeUnionSet :: forall m. MonadIO m
  => UnionSet %1 -> IslT m (Ur [SomeDisjunction], UnionSet)
decomposeUnionSet = unsafeCoerce go
  where
    go :: UnionSet -> IslT m (Ur [SomeDisjunction], UnionSet)
    go (UnionSet rawUs) = do
      let !(ref, rawUs') = borrow rawUs (\r -> r)
      results <- unsafeIslFromIO $ \_ ->
        Foreach.unionSetForeachSet ref $ \s -> do
          let !(sRef, _) = borrow s (\r -> r)
          space <- Foreach.setGetSpace sRef
          nDims <- Foreach.spaceDim space Isl.islDimSet
          nParams <- Foreach.spaceDim space Isl.islDimParam
          Foreach.spaceFree space
          conjunctions <- Foreach.setForeachBasicSet sRef $ \bs -> do
            let !(bsRef, _) = borrow bs (\r -> r)
            constraints <- Foreach.basicSetForeachConstraint bsRef $ \c -> do
              result <- extractSetConstraint nParams nDims c
              Foreach.constraintFree c
              return result
            Foreach.basicSetFree bs
            return (Conjunction constraints)
          Foreach.setFree s
          return (nDims, conjunctions)
      return (Ur (map wrapDisjunction results), UnionSet rawUs')

wrapDisjunction :: (Int, [Conjunction SetIx]) -> SomeDisjunction
wrapDisjunction (nDims, conjs) =
  case someNatVal (fromIntegral nDims) of
    Just (SomeNat (_ :: proxy n)) ->
      -- Wrap with empty params for now; full param recovery is a TODO
      SomeDisjunction (unsafeCoerce (PDisjunction (map PConjunction conjs)) :: PDisjunction '[] n)
    Nothing -> error "wrapDisjunction: negative dimension count"

-- Apply (schedule application)

-- | Apply a union map to a union set. This is the core operation for
-- computing scheduled domains: @apply domain schedule@ gives the image
-- of @domain@ under @schedule@.
--
-- Both arguments are consumed (__isl_take).
apply :: forall m. MonadIO m => UnionSet %1 -> Isl.UnionMap %1 -> IslT m UnionSet
apply = unsafeCoerce go
  where
    go :: UnionSet -> Isl.UnionMap -> IslT m UnionSet
    go (UnionSet us) um = UnionSet <$> withCtx (US.apply us um)

-- Named decomposition

-- | Decompose a UnionSet into per-space 'NamedSet's, preserving tuple names
-- and parameter names from the ISL spaces. This is the primary interface for
-- multi-statement programs where tuple names identify statements.
--
-- Borrows the UnionSet (returned alongside the result).
decomposeUnionSetNamed :: forall m. MonadIO m
  => UnionSet %1 -> IslT m (Ur [NamedSet], UnionSet)
decomposeUnionSetNamed = unsafeCoerce go
  where
    go :: UnionSet -> IslT m (Ur [NamedSet], UnionSet)
    go (UnionSet rawUs) = do
      let !(ref, rawUs') = borrow rawUs (\r -> r)
      results <- unsafeIslFromIO $ \_ ->
        Foreach.unionSetForeachSet ref $ \s -> do
          let !(sRef, _) = borrow s (\r -> r)
          space <- Foreach.setGetSpace sRef
          nDims <- Foreach.spaceDim space Isl.islDimSet
          nParams <- Foreach.spaceDim space Isl.islDimParam
          tupleName <- Foreach.spaceGetTupleName space Isl.islDimSet
          paramNames <- forM [0 .. nParams - 1] $ \i ->
            Foreach.spaceGetDimName space Isl.islDimParam i
          Foreach.spaceFree space
          conjunctions <- Foreach.setForeachBasicSet sRef $ \bs -> do
            let !(bsRef, _) = borrow bs (\r -> r)
            constraints <- Foreach.basicSetForeachConstraint bsRef $ \c -> do
              result <- extractSetConstraint nParams nDims c
              Foreach.constraintFree c
              return result
            Foreach.basicSetFree bs
            return (Conjunction constraints)
          Foreach.setFree s
          return NamedSet
            { nsName   = tupleName
            , nsParams = catMaybes paramNames
            , nsNDims  = nDims
            , nsConjs  = conjunctions
            }
      return (Ur results, UnionSet rawUs')

-- Resource management

freeUnionSet :: forall m. MonadIO m => UnionSet %1 -> IslT m ()
freeUnionSet = unsafeCoerce go
  where
    go :: UnionSet -> IslT m ()
    go (UnionSet us) = freeM us

-- Consuming combinators

-- | Borrow a UnionSet for a query, then free it.
consumingUS :: forall m a. MonadIO m => UnionSet %1 -> (UnionSetRef -> a) -> IslT m (Ur a)
consumingUS = unsafeCoerce go
  where
    go :: UnionSet -> (UnionSetRef -> a) -> IslT m (Ur a)
    go (UnionSet us) f = do
      let !(result, us') = borrow us (\ref -> f (UnionSetRef ref))
      freeM us'
      return (Ur result)

-- | Check if a UnionSet is empty, then free it.
consumingIsEmpty :: forall m. MonadIO m => UnionSet %1 -> IslT m (Ur Bool)
consumingIsEmpty = unsafeCoerce go
  where
    go :: UnionSet -> IslT m (Ur Bool)
    go (UnionSet us) = do
      r <- withCtx (US.isEmpty us)
      freeM us
      return (Ur r)

-- | Check equality of two UnionSets, then free both.
consumingIsEqual :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m (Ur Bool)
consumingIsEqual = unsafeCoerce go
  where
    go :: UnionSet -> UnionSet -> IslT m (Ur Bool)
    go (UnionSet us1) (UnionSet us2) = do
      r <- withCtx (US.isEqual us1 us2)
      freeM us1; freeM us2
      return (Ur r)

-- | Check if first UnionSet is a subset of the second, then free both.
consumingIsSubset :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m (Ur Bool)
consumingIsSubset = unsafeCoerce go
  where
    go :: UnionSet -> UnionSet -> IslT m (Ur Bool)
    go (UnionSet us1) (UnionSet us2) = do
      r <- withCtx (US.isSubset us1 us2)
      freeM us1; freeM us2
      return (Ur r)
