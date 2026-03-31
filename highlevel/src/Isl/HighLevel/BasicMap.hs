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

module Isl.HighLevel.BasicMap where

import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO)
import Data.Proxy
import GHC.TypeLits
import Control.Exception (evaluate)
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.BasicSet (BasicSet(..))
import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Params (KnownSymbols(..), Length)
import Isl.HighLevel.Pure (PMapConjunction(..))

import qualified Isl.Types as Isl
import Isl.Types (Borrow(..), Dupable(..), Consumable(..))
import Isl.Monad (IslT(..), Ur(..), unsafeIslFromIO, freeM)
import qualified Isl.BasicMap as BM
import qualified Isl.Space as Space

-- | Owned, parameter- and dimension-indexed BasicMap.
-- Linear — must be consumed exactly once.
newtype BasicMap (ps :: [Symbol]) (nIn :: Nat) (nOut :: Nat) = BasicMap Isl.BasicMap

-- | Borrowed reference to a BasicMap. Unrestricted — safe for queries.
newtype BasicMapRef (ps :: [Symbol]) (nIn :: Nat) (nOut :: Nat) = BasicMapRef Isl.BasicMapRef

instance Consumable (BasicMap ps ni no) where
  consume = unsafeCoerce $ \(BasicMap bm) -> consume bm

instance Dupable (BasicMap ps ni no) where
  dup = unsafeCoerce $ \(BasicMap bm) ->
    let (a, b) = dup bm in (BasicMap a, BasicMap b)

instance Borrow (BasicMap ps ni no) (BasicMapRef ps ni no) where
  borrow = unsafeCoerce $ \(BasicMap bm) f ->
    let (result, bm') = borrow bm (\ref -> f (BasicMapRef ref))
    in (result, BasicMap bm')

-- Construction

mkBasicMap
  :: forall ps ni no m. (MonadIO m, KnownNat ni, KnownNat no, KnownSymbols ps, KnownNat (Length ps))
  => (IxList (Length ps) MapIx -> IxList ni MapIx -> IxList no MapIx -> Conjunction MapIx)
  -> IslT m (BasicMap ps ni no)
mkBasicMap mkConstraints = toBasicMap @ps conj
  where
    nParams = natVal (Proxy @(Length ps))
    paramList = coerceIxList $ mkIxListWith MapParam 0 nParams
    inList  = coerceIxList $ mkIxListWith InDim  0 (natVal (Proxy @ni))
    outList = coerceIxList $ mkIxListWith OutDim 0 (natVal (Proxy @no))
    conj = mkConstraints paramList inList outList

toBasicMap
  :: forall ps m ni no. (MonadIO m, KnownNat ni, KnownNat no, KnownSymbols ps, KnownNat (Length ps))
  => Conjunction MapIx -> IslT m (BasicMap ps ni no)
toBasicMap (Conjunction constraints) = do
  let nParams = fromIntegral $ natVal (Proxy @(Length ps))
      nIn  = fromIntegral $ natVal (Proxy @ni)
      nOut = fromIntegral $ natVal (Proxy @no)
      paramNames = symbolVals @ps
  space0 <- Space.alloc nParams nIn nOut
  space <- foldM (\sp (i, name) -> Space.setDimName sp Isl.islDimParam i name)
                 space0
                 (zip [0..] paramNames)
  univ <- BM.universe space
  result <- foldM addMapConstraint univ constraints
  return (BasicMap result)

fromString :: forall m ps ni no. MonadIO m => String -> IslT m (BasicMap ps ni no)
fromString str = BasicMap <$> BM.readFromStr str

-- Operations (consuming)

intersect :: forall m ps ni no. MonadIO m => BasicMap ps ni no %1 -> BasicMap ps ni no %1 -> IslT m (BasicMap ps ni no)
intersect = unsafeCoerce go
  where
    go :: BasicMap ps ni no -> BasicMap ps ni no -> IslT m (BasicMap ps ni no)
    go (BasicMap bm1) (BasicMap bm2) = BasicMap <$> BM.intersect bm1 bm2

domain :: forall m ps ni no. MonadIO m => BasicMap ps ni no %1 -> IslT m (BasicSet ps ni)
domain = unsafeCoerce go
  where
    go :: BasicMap ps ni no -> IslT m (BasicSet ps ni)
    go (BasicMap bm) = BasicSet <$> BM.domain bm

range :: forall m ps ni no. MonadIO m => BasicMap ps ni no %1 -> IslT m (BasicSet ps no)
range = unsafeCoerce go
  where
    go :: BasicMap ps ni no -> IslT m (BasicSet ps no)
    go (BasicMap bm) = BasicSet <$> BM.range bm

-- Predicates (borrowing)

isEmpty :: forall m ps ni no. Monad m => BasicMap ps ni no %1 -> IslT m (Ur Bool, BasicMap ps ni no)
isEmpty = unsafeCoerce go
  where
    go :: BasicMap ps ni no -> IslT m (Ur Bool, BasicMap ps ni no)
    go (BasicMap bm) =
      let !(r, bm') = borrow bm (\ref -> BM.isEmpty ref)
      in IslT $ \_ -> return (Ur r, BasicMap bm')

isEqual :: forall m ps ni no. Monad m
  => BasicMap ps ni no %1 -> BasicMap ps ni no %1 -> IslT m (Ur Bool, BasicMap ps ni no, BasicMap ps ni no)
isEqual = unsafeCoerce go
  where
    go :: BasicMap ps ni no -> BasicMap ps ni no -> IslT m (Ur Bool, BasicMap ps ni no, BasicMap ps ni no)
    go (BasicMap bm1) (BasicMap bm2) =
      let !(r, bm1') = borrow bm1 (\ref1 ->
            let !(r', bm2') = borrow bm2 (\ref2 -> BM.isEqual ref1 ref2)
            in (r', bm2'))
          (innerR, bm2') = r
      in IslT $ \_ -> return (Ur innerR, BasicMap bm1', BasicMap bm2')

-- Queries

bmapToString :: BasicMapRef ps ni no -> String
bmapToString (BasicMapRef bmRef) = BM.toStr bmRef

borrowBM :: forall m ps ni no a. Monad m
  => BasicMap ps ni no %1 -> (BasicMapRef ps ni no -> a) -> IslT m (Ur a, BasicMap ps ni no)
borrowBM = unsafeCoerce go
  where
    go :: BasicMap ps ni no -> (BasicMapRef ps ni no -> a) -> IslT m (Ur a, BasicMap ps ni no)
    go (BasicMap bm) f =
      let !(result, bm') = borrow bm (\ref -> f (BasicMapRef ref))
      in IslT $ \_ -> return (Ur result, BasicMap bm')

-- Decomposition

decomposeBM :: forall m ps ni no. (MonadIO m, KnownNat ni, KnownNat no, KnownSymbols ps, KnownNat (Length ps))
  => BasicMap ps ni no %1 -> IslT m (Ur (PMapConjunction ps ni no), BasicMap ps ni no)
decomposeBM = unsafeCoerce go
  where
    nParams = fromIntegral $ natVal (Proxy @(Length ps))
    nIn  = fromIntegral $ natVal (Proxy @ni)
    nOut = fromIntegral $ natVal (Proxy @no)

    go :: BasicMap ps ni no -> IslT m (Ur (PMapConjunction ps ni no), BasicMap ps ni no)
    go (BasicMap rawBm) = do
      let !(ref, rawBm') = borrow rawBm (\r -> r)
          !bmRef = Isl.BasicMapRef (Isl.unBasicMap rawBm)
      constraints <- unsafeIslFromIO $ \_ -> do
        divExprs <- extractMapDivs bmRef nIn nOut nParams
        BM.foreachConstraint ref $ \c -> do
          result <- extractMapConstraint nParams nIn nOut divExprs c
          evaluate (consume c)
          return result
      return (Ur (PMapConjunction (Conjunction constraints)), BasicMap rawBm')

-- Resource management

freeBM :: forall m ps ni no. MonadIO m => BasicMap ps ni no %1 -> IslT m ()
freeBM = unsafeCoerce go
  where
    go :: BasicMap ps ni no -> IslT m ()
    go (BasicMap bm) = freeM bm

-- Consuming combinators

-- | Borrow a BasicMap for a query, then free it.
consumingBM :: forall m ps ni no a. MonadIO m => BasicMap ps ni no %1 -> (BasicMapRef ps ni no -> a) -> IslT m (Ur a)
consumingBM = unsafeCoerce go
  where
    go :: BasicMap ps ni no -> (BasicMapRef ps ni no -> a) -> IslT m (Ur a)
    go (BasicMap bm) f = do
      let !(result, bm') = borrow bm (\ref -> f (BasicMapRef ref))
      freeM bm'
      return (Ur result)

-- | Check if a BasicMap is empty, then free it.
consumingIsEmpty :: forall m ps ni no. MonadIO m => BasicMap ps ni no %1 -> IslT m (Ur Bool)
consumingIsEmpty = unsafeCoerce go
  where
    go :: BasicMap ps ni no -> IslT m (Ur Bool)
    go (BasicMap bm) = do
      let !(r, bm') = borrow bm (\ref -> BM.isEmpty ref)
      freeM bm'
      return (Ur r)

-- | Check equality of two BasicMaps, then free both.
consumingIsEqual :: forall m ps ni no. MonadIO m => BasicMap ps ni no %1 -> BasicMap ps ni no %1 -> IslT m (Ur Bool)
consumingIsEqual = unsafeCoerce go
  where
    go :: BasicMap ps ni no -> BasicMap ps ni no -> IslT m (Ur Bool)
    go (BasicMap bm1) (BasicMap bm2) = do
      let !(r, bm1') = borrow bm1 (\ref1 ->
            let !(r', bm2') = borrow bm2 (\ref2 -> BM.isEqual ref1 ref2)
            in (r', bm2'))
          (innerR, bm2') = r
      freeM bm1'; freeM bm2'
      return (Ur innerR)
