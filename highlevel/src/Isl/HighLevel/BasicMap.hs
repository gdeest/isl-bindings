{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Isl.HighLevel.BasicMap where

import Control.Monad (forM, foldM)
import Control.Monad.IO.Class (MonadIO)
import Data.Proxy
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.BasicSet (BasicSet(..))
import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Pure (PMapConjunction(..))

import qualified Isl.Types as Isl
import Isl.Types (Borrow(..), Dupable(..), Consumable(..))
import Isl.Instances ()
import Isl.Monad (IslT(..), Ur(..), unsafeIslFromIO, withCtx, freeM)
import qualified Isl.Foreach as Foreach
import qualified Isl.BasicMap.AutoGen as BM
import qualified Isl.Constraint.AutoGen as Constraint
import qualified Isl.LocalSpace.AutoGen as LS
import qualified Isl.Space.AutoGen as Space

-- | Owned, dimension-indexed BasicMap. Linear — must be consumed exactly once.
newtype BasicMap (nIn :: Nat) (nOut :: Nat) = BasicMap Isl.BasicMap

-- | Borrowed reference to a BasicMap. Unrestricted — safe for queries.
newtype BasicMapRef (nIn :: Nat) (nOut :: Nat) = BasicMapRef Isl.BasicMapRef

instance Consumable (BasicMap ni no) where
  consume = unsafeCoerce $ \(BasicMap bm) -> consume bm

instance Dupable (BasicMap ni no) where
  dup = unsafeCoerce $ \(BasicMap bm) ->
    let (a, b) = dup bm in (BasicMap a, BasicMap b)

instance Borrow (BasicMap ni no) (BasicMapRef ni no) where
  borrow = unsafeCoerce $ \(BasicMap bm) f ->
    let (result, bm') = borrow bm (\ref -> f (BasicMapRef ref))
    in (result, BasicMap bm')

-- Construction

mkBasicMap
  :: forall ni no m. (MonadIO m, KnownNat ni, KnownNat no)
  => (IxList ni MapDim -> IxList no MapDim -> Conjunction MapDim)
  -> IslT m (BasicMap ni no)
mkBasicMap mkConstraints = toBasicMap $ mkConstraints inList outList
  where
    inList  = coerceIxList $ mkIxListWith InDim  0 (natVal (Proxy @ni))
    outList = coerceIxList $ mkIxListWith OutDim 0 (natVal (Proxy @no))

toBasicMap
  :: forall m ni no. (MonadIO m, KnownNat ni, KnownNat no)
  => Conjunction MapDim -> IslT m (BasicMap ni no)
toBasicMap (Conjunction constraints) = do
  let nIn  = fromIntegral $ natVal (Proxy @ni)
      nOut = fromIntegral $ natVal (Proxy @no)
  space <- withCtx $ Space.alloc 0 nIn nOut
  univ <- withCtx $ BM.universe space
  result <- foldM addConstraint univ constraints
  return (BasicMap result)
  where
    getSpace :: Isl.BasicMapRef -> Isl.Space
    getSpace ref = unsafePerformIO $ Foreach.basicMapGetSpace ref
    addConstraint bm constraint = do
      let !(sp, bm') = borrow bm (unsafeCoerce getSpace)
      ls <- withCtx $ LS.fromSpace sp
      (emptyC, e) <-
        case constraint of
          InequalityConstraint e -> do
            co <- withCtx $ Constraint.inequalityAlloc ls
            return (co, e)
          EqualityConstraint e -> do
            co <- withCtx $ Constraint.equalityAlloc ls
            return (co, e)
      let (coeffs, constant) = expandExpr e
          setCoeff constr (coeff, dim) = do
            let (dimType, pos) = case dim of
                  InDim i  -> (Isl.islDimIn, i)
                  OutDim j -> (Isl.islDimOut, j)
            withCtx $ Constraint.setCoefficientSi
              constr dimType (fromIntegral pos) (fromIntegral coeff)
      linearPart <- foldM setCoeff emptyC coeffs
      finalC <- withCtx $ Constraint.setConstantSi linearPart (fromIntegral constant)
      withCtx $ BM.addConstraint bm' finalC

fromString :: forall m ni no. MonadIO m => String -> IslT m (BasicMap ni no)
fromString str = BasicMap <$> withCtx (BM.readFromStr str)

-- Operations (consuming)

intersect :: forall m ni no. MonadIO m => BasicMap ni no %1 -> BasicMap ni no %1 -> IslT m (BasicMap ni no)
intersect = unsafeCoerce go
  where
    go :: BasicMap ni no -> BasicMap ni no -> IslT m (BasicMap ni no)
    go (BasicMap bm1) (BasicMap bm2) = BasicMap <$> withCtx (BM.intersect bm1 bm2)

domain :: forall m ni no. MonadIO m => BasicMap ni no %1 -> IslT m (BasicSet ni)
domain = unsafeCoerce go
  where
    go :: BasicMap ni no -> IslT m (BasicSet ni)
    go (BasicMap bm) = BasicSet <$> withCtx (BM.domain bm)

range :: forall m ni no. MonadIO m => BasicMap ni no %1 -> IslT m (BasicSet no)
range = unsafeCoerce go
  where
    go :: BasicMap ni no -> IslT m (BasicSet no)
    go (BasicMap bm) = BasicSet <$> withCtx (BM.range bm)

-- Predicates (borrowing)

isEmpty :: forall m ni no. Monad m => BasicMap ni no %1 -> IslT m (Ur Bool, BasicMap ni no)
isEmpty = unsafeCoerce go
  where
    go :: BasicMap ni no -> IslT m (Ur Bool, BasicMap ni no)
    go (BasicMap bm) = do
      r <- withCtx (BM.isEmpty bm)
      return (Ur r, BasicMap bm)

isEqual :: forall m ni no. Monad m
  => BasicMap ni no %1 -> BasicMap ni no %1 -> IslT m (Ur Bool, BasicMap ni no, BasicMap ni no)
isEqual = unsafeCoerce go
  where
    go :: BasicMap ni no -> BasicMap ni no -> IslT m (Ur Bool, BasicMap ni no, BasicMap ni no)
    go (BasicMap bm1) (BasicMap bm2) = do
      r <- withCtx (BM.isEqual bm1 bm2)
      return (Ur r, BasicMap bm1, BasicMap bm2)

-- Queries

bmapToString :: BasicMapRef ni no -> String
bmapToString (BasicMapRef bmRef) = unsafePerformIO $ Foreach.basicMapToStr bmRef

borrowBM :: forall m ni no a. Monad m
  => BasicMap ni no %1 -> (BasicMapRef ni no -> a) -> IslT m (Ur a, BasicMap ni no)
borrowBM = unsafeCoerce go
  where
    go :: BasicMap ni no -> (BasicMapRef ni no -> a) -> IslT m (Ur a, BasicMap ni no)
    go (BasicMap bm) f =
      let !(result, bm') = borrow bm (\ref -> f (BasicMapRef ref))
      in IslT $ \_ -> return (Ur result, BasicMap bm')

-- Decomposition

decomposeBM :: forall m ni no. (MonadIO m, KnownNat ni, KnownNat no)
  => BasicMap ni no %1 -> IslT m (Ur (PMapConjunction ni no), BasicMap ni no)
decomposeBM = unsafeCoerce go
  where
    nIn  = fromIntegral $ natVal (Proxy @ni)
    nOut = fromIntegral $ natVal (Proxy @no)

    go :: BasicMap ni no -> IslT m (Ur (PMapConjunction ni no), BasicMap ni no)
    go (BasicMap rawBm) = do
      let !(ref, rawBm') = borrow rawBm (\r -> r)
      constraints <- unsafeIslFromIO $ \_ ->
        Foreach.basicMapForeachConstraint ref $ \c -> do
          result <- extractMapConstraint nIn nOut c
          Foreach.constraintFree c
          return result
      return (Ur (PMapConjunction (Conjunction constraints)), BasicMap rawBm')

-- | Extract a pure 'Constraint' from a raw ISL constraint,
-- reading coefficients for both input and output dimensions.
extractMapConstraint :: Int -> Int -> Isl.Constraint -> IO (Constraint MapDim)
extractMapConstraint nIn nOut c = do
  isEq <- Foreach.constraintIsEquality c
  inCoeffs <- forM [0 .. nIn - 1] $ \i -> do
    coeff <- Foreach.constraintGetCoefficientSi c Isl.islDimIn i
    return (coeff, InDim i)
  outCoeffs <- forM [0 .. nOut - 1] $ \j -> do
    coeff <- Foreach.constraintGetCoefficientSi c Isl.islDimOut j
    return (coeff, OutDim j)
  constant <- Foreach.constraintGetConstantSi c
  let allCoeffs = filter (\(coeff, _) -> coeff /= 0) (inCoeffs ++ outCoeffs)
      expr = rebuildExpr allCoeffs constant
  return $ if isEq
    then EqualityConstraint expr
    else InequalityConstraint expr

-- Resource management

freeBM :: forall m ni no. MonadIO m => BasicMap ni no %1 -> IslT m ()
freeBM = unsafeCoerce go
  where
    go :: BasicMap ni no -> IslT m ()
    go (BasicMap bm) = freeM bm
