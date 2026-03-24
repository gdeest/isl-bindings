{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Isl.HighLevel.UnionMap where

import Control.Monad.IO.Class (MonadIO)
import GHC.TypeLits (someNatVal, SomeNat(..), KnownNat, Symbol)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.BasicMap (extractMapConstraint)
import Isl.HighLevel.Constraints (Conjunction(..), MapIx)
import Isl.HighLevel.Map (Map(..))
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
