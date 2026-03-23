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
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.Constraints
import Isl.HighLevel.Context
import Isl.HighLevel.Indices

import qualified Isl.Types as Isl
import Isl.Types (Borrow(..), Dupable(..), Consumable(..))
import Isl.Monad (IslT(..), Ur(..), unsafeIslFromIO, freeM)
import Isl.Linear (borrowPure)

import qualified Isl.BasicSet.AutoGen as BS
import qualified Isl.Constraint.AutoGen as Constraint
import qualified Isl.LocalSpace.AutoGen as LS
import qualified Isl.Space.AutoGen as Space

-- | Owned, dimension-indexed BasicSet. Linear — must be consumed exactly once.
newtype BasicSet (nDims :: Nat) = BasicSet Isl.BasicSet

-- | Borrowed reference to a BasicSet. Unrestricted — safe for queries.
newtype BasicSetRef (nDims :: Nat) = BasicSetRef Isl.BasicSetRef

-- Linearity primitives. Uses unsafeCoerce because pattern-matching on the
-- newtype would violate the linearity checker, but the operations are safe:
-- consume calls isl_free, dup calls isl_copy, borrow shares the pointer.

instance Consumable (BasicSet n) where
  consume = unsafeCoerce $ \(BasicSet bs) -> consume bs

instance Dupable (BasicSet n) where
  dup = unsafeCoerce $ \(BasicSet bs) ->
    let (a, b) = dup bs in (BasicSet a, BasicSet b)

instance Borrow (BasicSet n) (BasicSetRef n) where
  borrow = unsafeCoerce $ \(BasicSet bs) f ->
    let (result, bs') = borrow bs (\ref -> f (BasicSetRef ref))
    in (result, BasicSet bs')

-- Construction

mkBasicSet
  :: forall (n :: Nat) m. (MonadIO m, KnownNat n)
  => (forall ix. IxList n ix -> Conjunction ix)
  -> IslT m (BasicSet n)
mkBasicSet mkConstraints = toBasicSet . mkConstraints $
  (coerceIxList $ mkIxList 0 $ natVal (Proxy @n))

toBasicSet
  :: forall m (n :: Nat). (MonadIO m, KnownNat n)
  => Conjunction Integer -> IslT m (BasicSet n)
toBasicSet (Conjunction constraints) = do
  space <- Space.setAlloc 0 (fromIntegral $ natVal $ Proxy @n)
  univ <- BS.universe space
  result <- foldM addConstraint univ constraints
  return (BasicSet result)
  where
    addConstraint bs constraint = do
      let (spAction, bs') = Isl.borrow bs BS.getSpace
      sp <- spAction
      ls <- LS.fromSpace sp
      (emptyC, e) <-
        case constraint of
          InequalityConstraint e -> do
            co <- Constraint.inequalityAlloc ls
            return (co, e)
          EqualityConstraint e -> do
            co <- Constraint.equalityAlloc ls
            return (co, e)
      let (coeffs, constant) = expandExpr e
          setCoeff constr (coeff, ix) =
            Constraint.setCoefficientSi
              constr Isl.islDimSet (fromIntegral ix) (fromIntegral coeff)
      linearPart <- foldM setCoeff emptyC coeffs
      finalC <- Constraint.setConstantSi linearPart (fromIntegral constant)
      BS.addConstraint bs' finalC

-- Operations (consuming)

intersect :: forall m n. MonadIO m => BasicSet n %1 -> BasicSet n %1 -> IslT m (BasicSet n)
intersect = unsafeCoerce go
  where
    go :: BasicSet n -> BasicSet n -> IslT m (BasicSet n)
    go (BasicSet bs1) (BasicSet bs2) = BasicSet <$> BS.intersect bs1 bs2

eliminateLast
  :: forall m n. (MonadIO m, KnownNat n, 1 <= n)
  => BasicSet n %1 -> IslT m (BasicSet (n-1))
eliminateLast = unsafeCoerce go
  where
    go :: BasicSet n -> IslT m (BasicSet (n-1))
    go (BasicSet bs) =
      let d = fromIntegral $ (natVal $ Proxy @n) - 1
      in BasicSet <$> BS.projectOut bs Isl.islDimSet d 1

fromString :: MonadIO m => String -> IslT m (BasicSet n)
fromString str = BasicSet <$> BS.readFromStr str

-- Queries (borrowing)

bsetToString :: BasicSetRef n -> String
bsetToString (BasicSetRef bsRef) = BS.toStr bsRef

-- | Borrow a BasicSet, apply a pure query to its Ref, return the result
-- as unrestricted (Ur) alongside the still-owned BasicSet.
borrowBS :: forall m n a. Monad m => BasicSet n %1 -> (BasicSetRef n -> a) -> IslT m (Ur a, BasicSet n)
borrowBS = unsafeCoerce go
  where
    go :: BasicSet n -> (BasicSetRef n -> a) -> IslT m (Ur a, BasicSet n)
    go (BasicSet bs) f =
      let !(result, bs') = borrow bs (\ref -> f (BasicSetRef ref))
      in IslT $ \_ -> return (Ur result, BasicSet bs')

-- Resource management

-- | Free a BasicSet within the IslT monad. Sequenced by monadic bind,
-- unlike 'consume' which uses unsafePerformIO and can be deferred.
freeBS :: forall m n. MonadIO m => BasicSet n %1 -> IslT m ()
freeBS = unsafeCoerce go
  where
    go :: BasicSet n -> IslT m ()
    go (BasicSet bs) = freeM bs
