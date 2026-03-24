{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Isl.HighLevel.BasicSet where

import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.Proxy
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.Constraints
import Isl.HighLevel.Context
import Isl.HighLevel.Indices
import Isl.HighLevel.Params (KnownSymbols(..), Length)
import Isl.HighLevel.Pure (PConjunction(..))

import qualified Isl.Types as Isl
import Isl.Types (Borrow(..), Dupable(..), Consumable(..))
import Isl.Instances ()
import Isl.Monad (IslT(..), Ur(..), unsafeIslFromIO, withCtx, freeM)
import Isl.Linear (borrowPure)
import qualified Isl.Foreach as Foreach

import qualified Isl.BasicSet.AutoGen as BS
import qualified Isl.Constraint.AutoGen as Constraint
import qualified Isl.LocalSpace.AutoGen as LS
import qualified Isl.Space.AutoGen as Space

-- | Owned, parameter- and dimension-indexed BasicSet.
-- Linear — must be consumed exactly once.
newtype BasicSet (ps :: [Symbol]) (nDims :: Nat) = BasicSet Isl.BasicSet

-- | Borrowed reference to a BasicSet. Unrestricted — safe for queries.
newtype BasicSetRef (ps :: [Symbol]) (nDims :: Nat) = BasicSetRef Isl.BasicSetRef

-- Linearity primitives. Uses unsafeCoerce because pattern-matching on the
-- newtype would violate the linearity checker, but the operations are safe:
-- consume calls isl_free, dup calls isl_copy, borrow shares the pointer.

instance Consumable (BasicSet ps n) where
  consume = unsafeCoerce $ \(BasicSet bs) -> consume bs

instance Dupable (BasicSet ps n) where
  dup = unsafeCoerce $ \(BasicSet bs) ->
    let (a, b) = dup bs in (BasicSet a, BasicSet b)

instance Borrow (BasicSet ps n) (BasicSetRef ps n) where
  borrow = unsafeCoerce $ \(BasicSet bs) f ->
    let (result, bs') = borrow bs (\ref -> f (BasicSetRef ref))
    in (result, BasicSet bs')

-- Construction

mkBasicSet
  :: forall ps (n :: Nat) m. (MonadIO m, KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => (forall ix. IxList (Length ps) ix -> IxList n ix -> Conjunction ix)
  -> IslT m (BasicSet ps n)
mkBasicSet mkConstraints = toBasicSet @ps @n conj
  where
    nParams = natVal (Proxy @(Length ps))
    nDims = natVal (Proxy @n)
    paramList = coerceIxList $ mkIxListWith SetParam 0 nParams
    dimList = coerceIxList $ mkIxListWith SetDim 0 nDims
    conj = mkConstraints paramList dimList

toBasicSet
  :: forall ps (n :: Nat) m. (MonadIO m, KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => Conjunction SetIx -> IslT m (BasicSet ps n)
toBasicSet (Conjunction constraints) = do
  let nParams = fromIntegral $ natVal (Proxy @(Length ps))
      nDims = fromIntegral $ natVal (Proxy @n)
      paramNames = symbolVals @ps
  space0 <- withCtx $ Space.setAlloc nParams nDims
  -- Set parameter names on the space
  space <- foldM (\sp (i, name) -> withCtx $ Space.setDimName sp Isl.islDimParam i name)
                 space0
                 (zip [0..] paramNames)
  univ <- withCtx $ BS.universe space
  result <- foldM addConstraint univ constraints
  return (BasicSet result)
  where
    getSpace :: Isl.BasicSetRef -> Isl.Space
    getSpace ref = unsafePerformIO $ Foreach.basicSetGetSpace ref
    addConstraint bs constraint = do
      let !(sp, bs') = borrow bs getSpace
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
          setCoeff constr (coeff, ix) = do
            let (dimType, pos) = case ix of
                  SetDim i  -> (Isl.islDimSet, i)
                  SetParam i -> (Isl.islDimParam, i)
            withCtx $ Constraint.setCoefficientSi
              constr dimType (fromIntegral pos) (fromIntegral coeff)
      linearPart <- foldM setCoeff emptyC coeffs
      finalC <- withCtx $ Constraint.setConstantSi linearPart (fromIntegral constant)
      withCtx $ BS.addConstraint bs' finalC

-- Operations (consuming)

intersect :: forall m ps n. MonadIO m => BasicSet ps n %1 -> BasicSet ps n %1 -> IslT m (BasicSet ps n)
intersect = unsafeCoerce go
  where
    go :: BasicSet ps n -> BasicSet ps n -> IslT m (BasicSet ps n)
    go (BasicSet bs1) (BasicSet bs2) = BasicSet <$> withCtx (BS.intersect bs1 bs2)

eliminateLast
  :: forall m ps n. (MonadIO m, KnownNat n, 1 <= n)
  => BasicSet ps n %1 -> IslT m (BasicSet ps (n-1))
eliminateLast = unsafeCoerce go
  where
    go :: BasicSet ps n -> IslT m (BasicSet ps (n-1))
    go (BasicSet bs) =
      let d = fromIntegral $ (natVal $ Proxy @n) - 1
      in BasicSet <$> withCtx (BS.projectOut bs Isl.islDimSet d 1)

fromString :: MonadIO m => String -> IslT m (BasicSet ps n)
fromString str = BasicSet <$> withCtx (BS.readFromStr str)

-- Queries (borrowing)

bsetToString :: BasicSetRef ps n -> String
bsetToString (BasicSetRef bsRef) =
  unsafePerformIO $ Foreach.basicSetToStr bsRef

-- | Borrow a BasicSet, apply a pure query to its Ref, return the result
-- as unrestricted (Ur) alongside the still-owned BasicSet.
borrowBS :: forall m ps n a. Monad m => BasicSet ps n %1 -> (BasicSetRef ps n -> a) -> IslT m (Ur a, BasicSet ps n)
borrowBS = unsafeCoerce go
  where
    go :: BasicSet ps n -> (BasicSetRef ps n -> a) -> IslT m (Ur a, BasicSet ps n)
    go (BasicSet bs) f =
      let !(result, bs') = borrow bs (\ref -> f (BasicSetRef ref))
      in IslT $ \_ -> return (Ur result, BasicSet bs')

-- Deconstruction

-- | Decompose a BasicSet into its pure constraint representation.
-- Borrows the BasicSet and returns both the pure data and the original.
--
-- The returned 'PConjunction' uses 'SetIx' indices for both
-- set dimensions and parameters.
decomposeBS :: forall m ps n. (MonadIO m, KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => BasicSet ps n %1 -> IslT m (Ur (PConjunction ps n), BasicSet ps n)
decomposeBS = unsafeCoerce go
  where
    nParams = fromIntegral $ natVal (Proxy @(Length ps))
    nDims = fromIntegral $ natVal (Proxy @n)

    go :: BasicSet ps n -> IslT m (Ur (PConjunction ps n), BasicSet ps n)
    go (BasicSet rawBs) = do
      let !(ref, rawBs') = borrow rawBs (\r -> r)
      constraints <- unsafeIslFromIO $ \_ ->
        Foreach.basicSetForeachConstraint ref $ \c -> do
          result <- extractSetConstraint nParams nDims c
          Foreach.constraintFree c
          return result
      return (Ur (PConjunction (Conjunction constraints)), BasicSet rawBs')

-- | Extract a pure 'Constraint' from a raw ISL constraint pointer,
-- reading coefficients for @nParams@ parameters and @nDims@ set dimensions.
extractSetConstraint :: Int -> Int -> Isl.Constraint -> IO (Constraint SetIx)
extractSetConstraint nParams nDims c = do
  isEq <- Foreach.constraintIsEquality c
  paramCoeffs <- forM [0 .. nParams - 1] $ \i -> do
    coeff <- Foreach.constraintGetCoefficientSi c Isl.islDimParam i
    return (coeff, SetParam (fromIntegral i))
  dimCoeffs <- forM [0 .. nDims - 1] $ \i -> do
    coeff <- Foreach.constraintGetCoefficientSi c Isl.islDimSet i
    return (coeff, SetDim (fromIntegral i))
  constant <- Foreach.constraintGetConstantSi c
  let allCoeffs = filter (\(coeff, _) -> coeff /= 0) (paramCoeffs ++ dimCoeffs)
      expr = rebuildExpr allCoeffs constant
  return $ if isEq
    then EqualityConstraint expr
    else InequalityConstraint expr

-- Resource management

-- | Free a BasicSet within the IslT monad. Sequenced by monadic bind,
-- unlike 'consume' which uses unsafePerformIO and can be deferred.
freeBS :: forall m ps n. MonadIO m => BasicSet ps n %1 -> IslT m ()
freeBS = unsafeCoerce go
  where
    go :: BasicSet ps n -> IslT m ()
    go (BasicSet bs) = freeM bs
