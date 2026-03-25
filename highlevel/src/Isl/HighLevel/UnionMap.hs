{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Isl.HighLevel.UnionMap where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (catMaybes)
import GHC.TypeLits (someNatVal, SomeNat(..), KnownNat, Symbol)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.BasicMap (extractMapConstraint)
import Isl.HighLevel.Constraints (Conjunction(..), MapIx)
import Isl.HighLevel.Map (Map(..))
import Isl.HighLevel.UnionSet (UnionSet(..))
import Isl.HighLevel.Pure

import qualified Isl.Types as Isl
import Isl.Types (Borrow(..), Dupable(..), Consumable(..))
import Isl.Instances ()
import Isl.Monad (IslT(..), Ur(..), unsafeIslFromIO, withCtx, freeM)
import qualified Isl.Foreach as Foreach
import qualified Isl.UnionMap.AutoGen as UM

-- | Owned UnionMap. Not dimension-indexed because a union can contain
-- maps of different spaces.
newtype UnionMap = UnionMap Isl.UnionMap

-- | Borrowed reference to a UnionMap.
newtype UnionMapRef = UnionMapRef Isl.UnionMapRef

instance Consumable UnionMap where
  consume = unsafeCoerce $ \(UnionMap um) -> consume um

instance Dupable UnionMap where
  dup = unsafeCoerce $ \(UnionMap um) ->
    let (a, b) = dup um in (UnionMap a, UnionMap b)

instance Borrow UnionMap UnionMapRef where
  borrow = unsafeCoerce $ \(UnionMap um) f ->
    let (result, um') = borrow um (\ref -> f (UnionMapRef ref))
    in (result, UnionMap um')

-- Construction

fromMap :: forall m (ps :: [Symbol]) ni no. MonadIO m => Map ps ni no %1 -> IslT m UnionMap
fromMap = unsafeCoerce go
  where
    go :: Map ps ni no -> IslT m UnionMap
    go (Map m) = UnionMap <$> withCtx (UM.fromMap m)

fromString :: forall m. MonadIO m => String -> IslT m UnionMap
fromString str = UnionMap <$> withCtx (UM.readFromStr str)

-- Operations (consuming)

union :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
union = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m UnionMap
    go (UnionMap um1) (UnionMap um2) = UnionMap <$> withCtx (UM.union um1 um2)

intersect :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
intersect = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m UnionMap
    go (UnionMap um1) (UnionMap um2) = UnionMap <$> withCtx (UM.intersect um1 um2)

subtract :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
subtract = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m UnionMap
    go (UnionMap um1) (UnionMap um2) = UnionMap <$> withCtx (UM.subtract um1 um2)

coalesce :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
coalesce = unsafeCoerce go
  where
    go :: UnionMap -> IslT m UnionMap
    go (UnionMap um) = UnionMap <$> withCtx (UM.coalesce um)

-- | Extract the domain of a union map as a union set. Consuming.
domain :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionSet
domain = unsafeCoerce go
  where
    go :: UnionMap -> IslT m UnionSet
    go (UnionMap um) = UnionSet <$> withCtx (UM.domain um)

-- | Extract the range of a union map as a union set. Consuming.
range :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionSet
range = unsafeCoerce go
  where
    go :: UnionMap -> IslT m UnionSet
    go (UnionMap um) = UnionSet <$> withCtx (UM.range um)

-- | Reverse a union map (swap domain and range). Consuming.
reverse :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
reverse = unsafeCoerce go
  where
    go :: UnionMap -> IslT m UnionMap
    go (UnionMap um) = UnionMap <$> withCtx (UM.reverse um)

-- | Compose two union maps on the domain side. Consuming.
applyDomain :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
applyDomain = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m UnionMap
    go (UnionMap um1) (UnionMap um2) = UnionMap <$> withCtx (UM.applyDomain um1 um2)

-- | Compose two union maps on the range side. Consuming.
applyRange :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
applyRange = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m UnionMap
    go (UnionMap um1) (UnionMap um2) = UnionMap <$> withCtx (UM.applyRange um1 um2)

-- | Flat range product of two union maps. Consuming.
flatRangeProduct :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
flatRangeProduct = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m UnionMap
    go (UnionMap um1) (UnionMap um2) = UnionMap <$> withCtx (UM.flatRangeProduct um1 um2)

-- | Lexicographic less-than relation between two union maps. Consuming.
lexLtUnionMap :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
lexLtUnionMap = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m UnionMap
    go (UnionMap um1) (UnionMap um2) = UnionMap <$> withCtx (UM.lexLtUnionMap um1 um2)

-- | Intersect the domain of a union map with a union set. Consuming.
intersectDomain :: forall m. MonadIO m => UnionMap %1 -> UnionSet %1 -> IslT m UnionMap
intersectDomain = unsafeCoerce go
  where
    go :: UnionMap -> UnionSet -> IslT m UnionMap
    go (UnionMap um) (UnionSet us) = UnionMap <$> withCtx (UM.intersectDomain um us)

-- | Simplify a union map given a context (another union map). Consuming.
gist :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
gist = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m UnionMap
    go (UnionMap um1) (UnionMap um2) = UnionMap <$> withCtx (UM.gist um1 um2)

-- | Simplify a union map given a domain context (a union set). Consuming.
gistDomain :: forall m. MonadIO m => UnionMap %1 -> UnionSet %1 -> IslT m UnionMap
gistDomain = unsafeCoerce go
  where
    go :: UnionMap -> UnionSet -> IslT m UnionMap
    go (UnionMap um) (UnionSet us) = UnionMap <$> withCtx (UM.gistDomain um us)

-- Predicates (borrowing)

isEmpty :: forall m. Monad m => UnionMap %1 -> IslT m (Ur Bool, UnionMap)
isEmpty = unsafeCoerce go
  where
    go :: UnionMap -> IslT m (Ur Bool, UnionMap)
    go (UnionMap um) = do
      r <- withCtx (UM.isEmpty um)
      return (Ur r, UnionMap um)

isEqual :: forall m. Monad m => UnionMap %1 -> UnionMap %1 -> IslT m (Ur Bool, UnionMap, UnionMap)
isEqual = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m (Ur Bool, UnionMap, UnionMap)
    go (UnionMap um1) (UnionMap um2) = do
      r <- withCtx (UM.isEqual um1 um2)
      return (Ur r, UnionMap um1, UnionMap um2)

isSubset :: forall m. Monad m => UnionMap %1 -> UnionMap %1 -> IslT m (Ur Bool, UnionMap, UnionMap)
isSubset = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m (Ur Bool, UnionMap, UnionMap)
    go (UnionMap um1) (UnionMap um2) = do
      r <- withCtx (UM.isSubset um1 um2)
      return (Ur r, UnionMap um1, UnionMap um2)

-- Queries

umapToString :: UnionMapRef -> String
umapToString (UnionMapRef umRef) = unsafePerformIO $ Foreach.unionMapToStr umRef

borrowUM :: forall m a. Monad m => UnionMap %1 -> (UnionMapRef -> a) -> IslT m (Ur a, UnionMap)
borrowUM = unsafeCoerce go
  where
    go :: UnionMap -> (UnionMapRef -> a) -> IslT m (Ur a, UnionMap)
    go (UnionMap um) f =
      let !(result, um') = borrow um (\ref -> f (UnionMapRef ref))
      in IslT $ \_ -> return (Ur result, UnionMap um')

-- Decomposition

decomposeUnionMap :: forall m. MonadIO m
  => UnionMap %1 -> IslT m (Ur [SomeMapDisjunction], UnionMap)
decomposeUnionMap = unsafeCoerce go
  where
    go :: UnionMap -> IslT m (Ur [SomeMapDisjunction], UnionMap)
    go (UnionMap rawUm) = do
      let !(ref, rawUm') = borrow rawUm (\r -> r)
      results <- unsafeIslFromIO $ \_ ->
        Foreach.unionMapForeachMap ref $ \m -> do
          let !(mRef, _) = borrow m (\r -> r)
          space <- Foreach.mapGetSpace mRef
          nIn  <- Foreach.spaceDim space Isl.islDimIn
          nOut <- Foreach.spaceDim space Isl.islDimOut
          nParams <- Foreach.spaceDim space Isl.islDimParam
          Foreach.spaceFree space
          conjunctions <- Foreach.mapForeachBasicMap mRef $ \bm -> do
            let !(bmRef, _) = borrow bm (\r -> r)
            constraints <- Foreach.basicMapForeachConstraint bmRef $ \c -> do
              result <- extractMapConstraint nParams nIn nOut c
              Foreach.constraintFree c
              return result
            Foreach.basicMapFree bm
            return (Conjunction constraints)
          Foreach.mapFree m
          return (nIn, nOut, conjunctions)
      return (Ur (map wrapMapDisjunction results), UnionMap rawUm')

wrapMapDisjunction :: (Int, Int, [Conjunction MapIx]) -> SomeMapDisjunction
wrapMapDisjunction (nIn, nOut, conjs) =
  case (someNatVal (fromIntegral nIn), someNatVal (fromIntegral nOut)) of
    (Just (SomeNat (_ :: proxy ni)), Just (SomeNat (_ :: proxy no))) ->
      SomeMapDisjunction (unsafeCoerce (PMapDisjunction (map PMapConjunction conjs)) :: PMapDisjunction '[] ni no)
    _ -> error "wrapMapDisjunction: negative dimension count"

-- Named decomposition

-- | Decompose a UnionMap into per-space 'NamedMap's, preserving domain tuple
-- names and parameter names. Used for extracting per-statement schedule inverses.
--
-- Borrows the UnionMap (returned alongside the result).
decomposeUnionMapNamed :: forall m. MonadIO m
  => UnionMap %1 -> IslT m (Ur [NamedMap], UnionMap)
decomposeUnionMapNamed = unsafeCoerce go
  where
    go :: UnionMap -> IslT m (Ur [NamedMap], UnionMap)
    go (UnionMap rawUm) = do
      let !(ref, rawUm') = borrow rawUm (\r -> r)
      results <- unsafeIslFromIO $ \_ ->
        Foreach.unionMapForeachMap ref $ \m -> do
          let !(mRef, _) = borrow m (\r -> r)
          space <- Foreach.mapGetSpace mRef
          nIn  <- Foreach.spaceDim space Isl.islDimIn
          nOut <- Foreach.spaceDim space Isl.islDimOut
          nParams <- Foreach.spaceDim space Isl.islDimParam
          domainName <- Foreach.spaceGetTupleName space Isl.islDimIn
          paramNames <- forM [0 .. nParams - 1] $ \i ->
            Foreach.spaceGetDimName space Isl.islDimParam i
          Foreach.spaceFree space
          conjunctions <- Foreach.mapForeachBasicMap mRef $ \bm -> do
            let !(bmRef, _) = borrow bm (\r -> r)
            constraints <- Foreach.basicMapForeachConstraint bmRef $ \c -> do
              result <- extractMapConstraint nParams nIn nOut c
              Foreach.constraintFree c
              return result
            Foreach.basicMapFree bm
            return (Conjunction constraints)
          Foreach.mapFree m
          return NamedMap
            { nmDomainName = domainName
            , nmParams     = catMaybes paramNames
            , nmNIn        = nIn
            , nmNOut       = nOut
            , nmConjs      = conjunctions
            }
      return (Ur results, UnionMap rawUm')

-- Resource management

freeUnionMap :: forall m. MonadIO m => UnionMap %1 -> IslT m ()
freeUnionMap = unsafeCoerce go
  where
    go :: UnionMap -> IslT m ()
    go (UnionMap um) = freeM um

-- Consuming combinators

-- | Borrow a UnionMap for a query, then free it.
consumingUM :: forall m a. MonadIO m => UnionMap %1 -> (UnionMapRef -> a) -> IslT m (Ur a)
consumingUM = unsafeCoerce go
  where
    go :: UnionMap -> (UnionMapRef -> a) -> IslT m (Ur a)
    go (UnionMap um) f = do
      let !(result, um') = borrow um (\ref -> f (UnionMapRef ref))
      freeM um'
      return (Ur result)

-- | Check if a UnionMap is empty, then free it.
consumingIsEmpty :: forall m. MonadIO m => UnionMap %1 -> IslT m (Ur Bool)
consumingIsEmpty = unsafeCoerce go
  where
    go :: UnionMap -> IslT m (Ur Bool)
    go (UnionMap um) = do
      r <- withCtx (UM.isEmpty um)
      freeM um
      return (Ur r)

-- | Check equality of two UnionMaps, then free both.
consumingIsEqual :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m (Ur Bool)
consumingIsEqual = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m (Ur Bool)
    go (UnionMap um1) (UnionMap um2) = do
      r <- withCtx (UM.isEqual um1 um2)
      freeM um1; freeM um2
      return (Ur r)

-- | Check if first UnionMap is a subset of the second, then free both.
consumingIsSubset :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m (Ur Bool)
consumingIsSubset = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m (Ur Bool)
    go (UnionMap um1) (UnionMap um2) = do
      r <- withCtx (UM.isSubset um1 um2)
      freeM um1; freeM um2
      return (Ur r)
