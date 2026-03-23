{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Isl.HighLevel.Map where

import Control.Monad.IO.Class (MonadIO)
import Data.Proxy
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.BasicMap (BasicMap(..), extractMapConstraint)
import Isl.HighLevel.Constraints (Conjunction(..), MapDim)
import Isl.HighLevel.Pure (PMapConjunction(..), PMapDisjunction(..))
import Isl.HighLevel.Set (Set(..))

import qualified Isl.Types as Isl
import Isl.Types (Borrow(..), Dupable(..), Consumable(..))
import Isl.Instances ()
import Isl.Monad (IslT(..), Ur(..), unsafeIslFromIO, withCtx, freeM)
import qualified Isl.Foreach as Foreach
import qualified Isl.Map.AutoGen as M

-- | Owned, dimension-indexed Map. Linear — must be consumed exactly once.
newtype Map (nIn :: Nat) (nOut :: Nat) = Map Isl.Map

-- | Borrowed reference to a Map. Unrestricted — safe for queries.
newtype MapRef (nIn :: Nat) (nOut :: Nat) = MapRef Isl.MapRef

instance Consumable (Map ni no) where
  consume = unsafeCoerce $ \(Map m) -> consume m

instance Dupable (Map ni no) where
  dup = unsafeCoerce $ \(Map m) ->
    let (a, b) = dup m in (Map a, Map b)

instance Borrow (Map ni no) (MapRef ni no) where
  borrow = unsafeCoerce $ \(Map m) f ->
    let (result, m') = borrow m (\ref -> f (MapRef ref))
    in (result, Map m')

-- Construction

fromBasicMap :: forall m ni no. MonadIO m => BasicMap ni no %1 -> IslT m (Map ni no)
fromBasicMap = unsafeCoerce go
  where
    go :: BasicMap ni no -> IslT m (Map ni no)
    go (BasicMap bm) = Map <$> withCtx (M.fromBasicMap bm)

fromString :: forall m ni no. MonadIO m => String -> IslT m (Map ni no)
fromString str = Map <$> withCtx (M.readFromStr str)

-- Operations (consuming)

union :: forall m ni no. MonadIO m => Map ni no %1 -> Map ni no %1 -> IslT m (Map ni no)
union = unsafeCoerce go
  where
    go :: Map ni no -> Map ni no -> IslT m (Map ni no)
    go (Map m1) (Map m2) = Map <$> withCtx (M.union m1 m2)

intersect :: forall m ni no. MonadIO m => Map ni no %1 -> Map ni no %1 -> IslT m (Map ni no)
intersect = unsafeCoerce go
  where
    go :: Map ni no -> Map ni no -> IslT m (Map ni no)
    go (Map m1) (Map m2) = Map <$> withCtx (M.intersect m1 m2)

subtract :: forall m ni no. MonadIO m => Map ni no %1 -> Map ni no %1 -> IslT m (Map ni no)
subtract = unsafeCoerce go
  where
    go :: Map ni no -> Map ni no -> IslT m (Map ni no)
    go (Map m1) (Map m2) = Map <$> withCtx (M.subtract m1 m2)

domain :: forall m ni no. MonadIO m => Map ni no %1 -> IslT m (Set ni)
domain = unsafeCoerce go
  where
    go :: Map ni no -> IslT m (Set ni)
    go (Map m) = Set <$> withCtx (M.domain m)

range :: forall m ni no. MonadIO m => Map ni no %1 -> IslT m (Set no)
range = unsafeCoerce go
  where
    go :: Map ni no -> IslT m (Set no)
    go (Map m) = Set <$> withCtx (M.range m)

coalesce :: forall m ni no. MonadIO m => Map ni no %1 -> IslT m (Map ni no)
coalesce = unsafeCoerce go
  where
    go :: Map ni no -> IslT m (Map ni no)
    go (Map m) = Map <$> withCtx (M.coalesce m)

-- Predicates (borrowing)

isEmpty :: forall m ni no. Monad m => Map ni no %1 -> IslT m (Ur Bool, Map ni no)
isEmpty = unsafeCoerce go
  where
    go :: Map ni no -> IslT m (Ur Bool, Map ni no)
    go (Map m) = do
      r <- withCtx (M.isEmpty m)
      return (Ur r, Map m)

isEqual :: forall m ni no. Monad m
  => Map ni no %1 -> Map ni no %1 -> IslT m (Ur Bool, Map ni no, Map ni no)
isEqual = unsafeCoerce go
  where
    go :: Map ni no -> Map ni no -> IslT m (Ur Bool, Map ni no, Map ni no)
    go (Map m1) (Map m2) = do
      r <- withCtx (M.isEqual m1 m2)
      return (Ur r, Map m1, Map m2)

isSubset :: forall m ni no. Monad m
  => Map ni no %1 -> Map ni no %1 -> IslT m (Ur Bool, Map ni no, Map ni no)
isSubset = unsafeCoerce go
  where
    go :: Map ni no -> Map ni no -> IslT m (Ur Bool, Map ni no, Map ni no)
    go (Map m1) (Map m2) = do
      r <- withCtx (M.isSubset m1 m2)
      return (Ur r, Map m1, Map m2)

-- Conversion

toUnionMap :: forall m ni no. MonadIO m => Map ni no %1 -> IslT m Isl.UnionMap
toUnionMap = unsafeCoerce go
  where
    go :: Map ni no -> IslT m Isl.UnionMap
    go (Map m) = withCtx (M.toUnionMap m)

-- Queries

mapToString :: MapRef ni no -> String
mapToString (MapRef mRef) = unsafePerformIO $ Foreach.mapToStr mRef

borrowMap :: forall m ni no a. Monad m
  => Map ni no %1 -> (MapRef ni no -> a) -> IslT m (Ur a, Map ni no)
borrowMap = unsafeCoerce go
  where
    go :: Map ni no -> (MapRef ni no -> a) -> IslT m (Ur a, Map ni no)
    go (Map m) f =
      let !(result, m') = borrow m (\ref -> f (MapRef ref))
      in IslT $ \_ -> return (Ur result, Map m')

-- Decomposition

decomposeMap :: forall m ni no. (MonadIO m, KnownNat ni, KnownNat no)
  => Map ni no %1 -> IslT m (Ur (PMapDisjunction ni no), Map ni no)
decomposeMap = unsafeCoerce go
  where
    nIn  = fromIntegral $ natVal (Proxy @ni)
    nOut = fromIntegral $ natVal (Proxy @no)

    go :: Map ni no -> IslT m (Ur (PMapDisjunction ni no), Map ni no)
    go (Map rawMap) = do
      let !(ref, rawMap') = borrow rawMap (\r -> r)
      conjunctions <- unsafeIslFromIO $ \_ ->
        Foreach.mapForeachBasicMap ref $ \bm -> do
          let !(bmRef, _) = borrow bm (\r -> r)
          constraints <- Foreach.basicMapForeachConstraint bmRef $ \c -> do
            result <- extractMapConstraint nIn nOut c
            Foreach.constraintFree c
            return result
          Foreach.basicMapFree bm
          return (PMapConjunction (Conjunction constraints))
      return (Ur (PMapDisjunction conjunctions), Map rawMap')

-- Resource management

freeMap :: forall m ni no. MonadIO m => Map ni no %1 -> IslT m ()
freeMap = unsafeCoerce go
  where
    go :: Map ni no -> IslT m ()
    go (Map m) = freeM m
