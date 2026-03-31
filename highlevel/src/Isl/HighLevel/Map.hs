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

module Isl.HighLevel.Map where

import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO)
import Data.Proxy
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.BasicMap (BasicMap(..))
import Isl.HighLevel.Constraints (Conjunction(..), MapIx, extractMapDivs, extractMapConstraint)
import Isl.HighLevel.Params (KnownSymbols(..), Length, Union)
import Isl.HighLevel.Pure (PMapConjunction(..), PMapDisjunction(..))
import Isl.HighLevel.Set (Set(..))

import qualified Isl.Types as Isl
import Isl.Types (Borrow(..), Dupable(..), Consumable(..))
import Isl.Monad (IslT(..), Ur(..), unsafeIslFromIO, freeM)
import qualified Isl.Map as M
import qualified Isl.Set as S
import qualified Isl.BasicMap as BM

-- | Owned, parameter- and dimension-indexed Map.
-- Linear — must be consumed exactly once.
newtype Map (ps :: [Symbol]) (nIn :: Nat) (nOut :: Nat) = Map Isl.Map

-- | Borrowed reference to a Map. Unrestricted — safe for queries.
newtype MapRef (ps :: [Symbol]) (nIn :: Nat) (nOut :: Nat) = MapRef Isl.MapRef

instance Consumable (Map ps ni no) where
  consume = unsafeCoerce $ \(Map m) -> consume m

instance Dupable (Map ps ni no) where
  dup = unsafeCoerce $ \(Map m) ->
    let (a, b) = dup m in (Map a, Map b)

instance Borrow (Map ps ni no) (MapRef ps ni no) where
  borrow = unsafeCoerce $ \(Map m) f ->
    let (result, m') = borrow m (\ref -> f (MapRef ref))
    in (result, Map m')

-- Construction

fromBasicMap :: forall m ps ni no. MonadIO m => BasicMap ps ni no %1 -> IslT m (Map ps ni no)
fromBasicMap = unsafeCoerce go
  where
    go :: BasicMap ps ni no -> IslT m (Map ps ni no)
    go (BasicMap bm) = Map <$> M.fromBasicMap bm

fromString :: forall m ps ni no. MonadIO m => String -> IslT m (Map ps ni no)
fromString str = Map <$> M.readFromStr str

-- Operations (consuming)

union :: forall m ps1 ps2 ni no. MonadIO m
  => Map ps1 ni no %1 -> Map ps2 ni no %1 -> IslT m (Map (Union ps1 ps2) ni no)
union = unsafeCoerce go
  where
    go :: Map ps1 ni no -> Map ps2 ni no -> IslT m (Map (Union ps1 ps2) ni no)
    go (Map m1) (Map m2) = Map <$> M.union m1 m2

intersect :: forall m ps1 ps2 ni no. MonadIO m
  => Map ps1 ni no %1 -> Map ps2 ni no %1 -> IslT m (Map (Union ps1 ps2) ni no)
intersect = unsafeCoerce go
  where
    go :: Map ps1 ni no -> Map ps2 ni no -> IslT m (Map (Union ps1 ps2) ni no)
    go (Map m1) (Map m2) = Map <$> M.intersect m1 m2

subtract :: forall m ps1 ps2 ni no. MonadIO m
  => Map ps1 ni no %1 -> Map ps2 ni no %1 -> IslT m (Map (Union ps1 ps2) ni no)
subtract = unsafeCoerce go
  where
    go :: Map ps1 ni no -> Map ps2 ni no -> IslT m (Map (Union ps1 ps2) ni no)
    go (Map m1) (Map m2) = Map <$> M.subtract m1 m2

domain :: forall m ps ni no. MonadIO m => Map ps ni no %1 -> IslT m (Set ps ni)
domain = unsafeCoerce go
  where
    go :: Map ps ni no -> IslT m (Set ps ni)
    go (Map m) = Set <$> M.domain m

range :: forall m ps ni no. MonadIO m => Map ps ni no %1 -> IslT m (Set ps no)
range = unsafeCoerce go
  where
    go :: Map ps ni no -> IslT m (Set ps no)
    go (Map m) = Set <$> M.range m

coalesce :: forall m ps ni no. MonadIO m => Map ps ni no %1 -> IslT m (Map ps ni no)
coalesce = unsafeCoerce go
  where
    go :: Map ps ni no -> IslT m (Map ps ni no)
    go (Map m) = Map <$> M.coalesce m

-- Reverse

reverse :: forall m ps ni no. MonadIO m => Map ps ni no %1 -> IslT m (Map ps no ni)
reverse = unsafeCoerce go
  where
    go :: Map ps ni no -> IslT m (Map ps no ni)
    go (Map m) = Map <$> M.reverse m

-- Complement

complement :: forall m ps ni no. MonadIO m => Map ps ni no %1 -> IslT m (Map ps ni no)
complement = unsafeCoerce go
  where
    go :: Map ps ni no -> IslT m (Map ps ni no)
    go (Map m) = Map <$> M.complement m

-- Apply (set through map)

apply :: forall m ps ni no. MonadIO m
  => Map ps ni no %1 -> Set ps ni %1 -> IslT m (Set ps no)
apply = unsafeCoerce go
  where
    go :: Map ps ni no -> Set ps ni -> IslT m (Set ps no)
    go (Map m) (Set s) = Set <$> S.apply s m

-- Composition

applyRange :: forall m ps ni k no. MonadIO m
  => Map ps ni k %1 -> Map ps k no %1 -> IslT m (Map ps ni no)
applyRange = unsafeCoerce go
  where
    go :: Map ps ni k -> Map ps k no -> IslT m (Map ps ni no)
    go (Map m1) (Map m2) = Map <$> M.applyRange m1 m2

applyDomain :: forall m ps ni k no. MonadIO m
  => Map ps ni k %1 -> Map ps no ni %1 -> IslT m (Map ps no k)
applyDomain = unsafeCoerce go
  where
    go :: Map ps ni k -> Map ps no ni -> IslT m (Map ps no k)
    go (Map m1) (Map m2) = Map <$> M.applyDomain m1 m2

-- Domain/range restriction

intersectDomain :: forall m ps ni no. MonadIO m
  => Map ps ni no %1 -> Set ps ni %1 -> IslT m (Map ps ni no)
intersectDomain = unsafeCoerce go
  where
    go :: Map ps ni no -> Set ps ni -> IslT m (Map ps ni no)
    go (Map m) (Set s) = Map <$> M.intersectDomain m s

intersectRange :: forall m ps ni no. MonadIO m
  => Map ps ni no %1 -> Set ps no %1 -> IslT m (Map ps ni no)
intersectRange = unsafeCoerce go
  where
    go :: Map ps ni no -> Set ps no -> IslT m (Map ps ni no)
    go (Map m) (Set s) = Map <$> M.intersectRange m s

subtractDomain :: forall m ps ni no. MonadIO m
  => Map ps ni no %1 -> Set ps ni %1 -> IslT m (Map ps ni no)
subtractDomain = unsafeCoerce go
  where
    go :: Map ps ni no -> Set ps ni -> IslT m (Map ps ni no)
    go (Map m) (Set s) = Map <$> M.subtractDomain m s

subtractRange :: forall m ps ni no. MonadIO m
  => Map ps ni no %1 -> Set ps no %1 -> IslT m (Map ps ni no)
subtractRange = unsafeCoerce go
  where
    go :: Map ps ni no -> Set ps no -> IslT m (Map ps ni no)
    go (Map m) (Set s) = Map <$> M.subtractRange m s

-- Products

flatProduct :: forall m ps ni1 no1 ni2 no2. MonadIO m
  => Map ps ni1 no1 %1 -> Map ps ni2 no2 %1 -> IslT m (Map ps (ni1 + ni2) (no1 + no2))
flatProduct = unsafeCoerce go
  where
    go :: Map ps ni1 no1 -> Map ps ni2 no2 -> IslT m (Map ps (ni1 + ni2) (no1 + no2))
    go (Map m1) (Map m2) = Map <$> M.flatProduct m1 m2

flatRangeProduct :: forall m ps ni no1 no2. MonadIO m
  => Map ps ni no1 %1 -> Map ps ni no2 %1 -> IslT m (Map ps ni (no1 + no2))
flatRangeProduct = unsafeCoerce go
  where
    go :: Map ps ni no1 -> Map ps ni no2 -> IslT m (Map ps ni (no1 + no2))
    go (Map m1) (Map m2) = Map <$> M.flatRangeProduct m1 m2

flatDomainProduct :: forall m ps ni1 ni2 no. MonadIO m
  => Map ps ni1 no %1 -> Map ps ni2 no %1 -> IslT m (Map ps (ni1 + ni2) no)
flatDomainProduct = unsafeCoerce go
  where
    go :: Map ps ni1 no -> Map ps ni2 no -> IslT m (Map ps (ni1 + ni2) no)
    go (Map m1) (Map m2) = Map <$> M.flatDomainProduct m1 m2

-- Predicates (borrowing)

isEmpty :: forall m ps ni no. Monad m => Map ps ni no %1 -> IslT m (Ur Bool, Map ps ni no)
isEmpty = unsafeCoerce go
  where
    go :: Map ps ni no -> IslT m (Ur Bool, Map ps ni no)
    go (Map m) =
      let !(r, m') = borrow m (\ref -> M.isEmpty ref)
      in return (Ur r, Map m')

isEqual :: forall m ps ni no. Monad m
  => Map ps ni no %1 -> Map ps ni no %1 -> IslT m (Ur Bool, Map ps ni no, Map ps ni no)
isEqual = unsafeCoerce go
  where
    go :: Map ps ni no -> Map ps ni no -> IslT m (Ur Bool, Map ps ni no, Map ps ni no)
    go (Map m1) (Map m2) =
      let !(ref1, m1') = borrow m1 (\r -> r)
          !(ref2, m2') = borrow m2 (\r -> r)
          !r = M.isEqual ref1 ref2
      in return (Ur r, Map m1', Map m2')

isSubset :: forall m ps ni no. Monad m
  => Map ps ni no %1 -> Map ps ni no %1 -> IslT m (Ur Bool, Map ps ni no, Map ps ni no)
isSubset = unsafeCoerce go
  where
    go :: Map ps ni no -> Map ps ni no -> IslT m (Ur Bool, Map ps ni no, Map ps ni no)
    go (Map m1) (Map m2) =
      let !(ref1, m1') = borrow m1 (\r -> r)
          !(ref2, m2') = borrow m2 (\r -> r)
          !r = M.isSubset ref1 ref2
      in return (Ur r, Map m1', Map m2')

-- Conversion

toUnionMap :: forall m ps ni no. MonadIO m => Map ps ni no %1 -> IslT m Isl.UnionMap
toUnionMap = unsafeCoerce go
  where
    go :: Map ps ni no -> IslT m Isl.UnionMap
    go (Map m) = M.toUnionMap m

-- Queries

mapToString :: MapRef ps ni no -> String
mapToString (MapRef mRef) = M.toStr mRef

borrowMap :: forall m ps ni no a. Monad m
  => Map ps ni no %1 -> (MapRef ps ni no -> a) -> IslT m (Ur a, Map ps ni no)
borrowMap = unsafeCoerce go
  where
    go :: Map ps ni no -> (MapRef ps ni no -> a) -> IslT m (Ur a, Map ps ni no)
    go (Map m) f =
      let !(result, m') = borrow m (\ref -> f (MapRef ref))
      in IslT $ \_ -> return (Ur result, Map m')

-- Decomposition

decomposeMap :: forall m ps ni no. (MonadIO m, KnownNat ni, KnownNat no, KnownSymbols ps, KnownNat (Length ps))
  => Map ps ni no %1 -> IslT m (Ur (PMapDisjunction ps ni no), Map ps ni no)
decomposeMap = unsafeCoerce go
  where
    nParams = fromIntegral $ natVal (Proxy @(Length ps))
    nIn  = fromIntegral $ natVal (Proxy @ni)
    nOut = fromIntegral $ natVal (Proxy @no)

    go :: Map ps ni no -> IslT m (Ur (PMapDisjunction ps ni no), Map ps ni no)
    go (Map rawMap) = do
      let !(ref, rawMap') = borrow rawMap (\r -> r)
      conjunctions <- unsafeIslFromIO $ \_ ->
        M.foreachBasicMap ref $ \bm -> do
          let !(bmRef, _) = borrow bm (\r -> r)
              !rawBmRef = Isl.BasicMapRef (Isl.unBasicMap bm)
          divExprs <- extractMapDivs rawBmRef nIn nOut nParams
          constraints <- BM.foreachConstraint bmRef $ \c -> do
            result <- extractMapConstraint nParams nIn nOut divExprs c
            evaluate (consume c)
            return result
          evaluate (consume bm)
          return (PMapConjunction (Conjunction constraints))
      return (Ur (PMapDisjunction conjunctions), Map rawMap')

-- Resource management

freeMap :: forall m ps ni no. MonadIO m => Map ps ni no %1 -> IslT m ()
freeMap = unsafeCoerce go
  where
    go :: Map ps ni no -> IslT m ()
    go (Map m) = freeM m

-- Consuming combinators

-- | Borrow a Map for a query, then free it.
consumingMap :: forall m ps ni no a. MonadIO m => Map ps ni no %1 -> (MapRef ps ni no -> a) -> IslT m (Ur a)
consumingMap = unsafeCoerce go
  where
    go :: Map ps ni no -> (MapRef ps ni no -> a) -> IslT m (Ur a)
    go (Map m) f = do
      let !(result, m') = borrow m (\ref -> f (MapRef ref))
      freeM m'
      return (Ur result)

-- | Check if a Map is empty, then free it.
consumingIsEmpty :: forall m ps ni no. MonadIO m => Map ps ni no %1 -> IslT m (Ur Bool)
consumingIsEmpty = unsafeCoerce go
  where
    go :: Map ps ni no -> IslT m (Ur Bool)
    go (Map m) = do
      let !(r, m') = borrow m (\ref -> M.isEmpty ref)
      freeM m'
      return (Ur r)

-- | Check equality of two Maps, then free both.
consumingIsEqual :: forall m ps ni no. MonadIO m => Map ps ni no %1 -> Map ps ni no %1 -> IslT m (Ur Bool)
consumingIsEqual = unsafeCoerce go
  where
    go :: Map ps ni no -> Map ps ni no -> IslT m (Ur Bool)
    go (Map m1) (Map m2) = do
      let !(ref1, m1') = borrow m1 (\r -> r)
          !(ref2, m2') = borrow m2 (\r -> r)
          !r = M.isEqual ref1 ref2
      freeM m1'; freeM m2'
      return (Ur r)

-- | Check if first Map is a subset of the second, then free both.
consumingIsSubset :: forall m ps ni no. MonadIO m => Map ps ni no %1 -> Map ps ni no %1 -> IslT m (Ur Bool)
consumingIsSubset = unsafeCoerce go
  where
    go :: Map ps ni no -> Map ps ni no -> IslT m (Ur Bool)
    go (Map m1) (Map m2) = do
      let !(ref1, m1') = borrow m1 (\r -> r)
          !(ref2, m2') = borrow m2 (\r -> r)
          !r = M.isSubset ref1 ref2
      freeM m1'; freeM m2'
      return (Ur r)
