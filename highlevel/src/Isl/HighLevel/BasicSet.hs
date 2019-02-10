{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Isl.HighLevel.BasicSet where

import Control.Monad
import Data.Proxy
import Data.Reflection
import Foreign.C.String
import GHC.TypeLits
import System.IO.Unsafe

import Isl.HighLevel.Constraints
import Isl.HighLevel.Context
import Isl.HighLevel.Indices

import qualified Isl.Types as Isl
import qualified Isl.BasicSet.AutoGen as BS
import qualified Isl.Constraint.AutoGen as Constraint
import qualified Isl.LocalSpace.AutoGen as LS
import qualified Isl.Space.AutoGen as Space

newtype BasicSet s (nDims :: Nat) = BasicSet Isl.BasicSet

mkBasicSet
  :: forall s (n :: Nat). (HasCtx s, KnownNat n)
  => (forall ix. IxList n ix -> Conjunction ix)
  -> BasicSet s n
mkBasicSet mkConstraints = toBasicSet . mkConstraints $
  (coerceIxList $ mkIxList 0 $ natVal (Proxy @n))

toBasicSet
  :: forall s (n :: Nat). (HasCtx s, KnownNat n)
  => Conjunction Integer -> BasicSet s n

toBasicSet (Conjunction constraints) = BasicSet $ unsafePerformIO $ do
  space <- Space.c_setAlloc ctx 0 (fromIntegral $ natVal $ Proxy @n)
  univ <- BS.c_universe space
  foldM addConstraint univ constraints
  where
    ctx = reflect (Proxy @s)
    addConstraint bs constraint = do
      sp <- BS.c_getSpace bs
      ls <- LS.c_fromSpace sp
      (emptyConstraint, e) <-
        case constraint of
          InequalityConstraint e -> do
            co <- Constraint.c_inequalityAlloc ls
            return (co, e)
          EqualityConstraint e -> do
            co <- Constraint.c_equalityAlloc ls
            return (co, e)
      let (coeffs, constant) = expandExpr e
          setCoeff constr (coeff, ix)  = do
            Constraint.c_setCoefficientSi
              constr Isl.islDimSet (fromIntegral ix) (fromIntegral coeff)

      linearPart <- foldM setCoeff emptyConstraint coeffs
      finalConstraint <-
        Constraint.c_setConstantSi linearPart (fromIntegral constant)

      BS.c_addConstraint bs finalConstraint

intersect :: forall s n. BasicSet s n -> BasicSet s n -> BasicSet s n
intersect (BasicSet bs1) (BasicSet bs2) = BasicSet $ unsafePerformIO $ do
  bs1' <- BS.c_copy bs1
  bs2' <- BS.c_copy bs2
  BS.c_intersect bs1' bs2'

bsetToString :: forall s n. BasicSet s n -> String
bsetToString (BasicSet bs) = unsafePerformIO $
  peekCString =<< BS.c_toStr bs

fromString :: forall s n. HasCtx s => String -> BasicSet s n
fromString str =
  let ctx = reflect $ Proxy @s in
    unsafePerformIO $ withCString str $ \cstr -> do
    BasicSet <$> BS.c_readFromStr ctx cstr

eliminateLast
  :: forall s n. (KnownNat n, 1 <= n)
  => BasicSet s n -> BasicSet s (n-1)
eliminateLast (BasicSet bs) = BasicSet $ unsafePerformIO $ do
  let d = (natVal $ Proxy @n) - 1
  bs' <- BS.c_copy bs
  BS.c_projectOut bs' Isl.islDimSet (fromIntegral $ d) 1
