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
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.Constraints
import Isl.HighLevel.Context
import Isl.HighLevel.Indices
import Isl.HighLevel.Params (KnownSymbols(..), Length)
import Isl.HighLevel.Pure (PConjunction(..))

import Control.Exception (evaluate)

import qualified Isl.Types as Isl
import Isl.Types (Borrow(..), Dupable(..), Consumable(..))
import Isl.Monad (IslT(..), Ur(..), unsafeIslFromIO, freeM)

import qualified Isl.BasicSet as BS
import qualified Isl.Space as Space

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
  space0 <- Space.setAlloc nParams nDims
  space <- foldM (\sp (i, name) -> Space.setDimName sp Isl.islDimParam i name)
                 space0
                 (zip [0..] paramNames)
  univ <- BS.universe space
  result <- foldM addSetConstraint univ constraints
  return (BasicSet result)

-- Operations (consuming)

intersect :: forall m ps n. MonadIO m => BasicSet ps n %1 -> BasicSet ps n %1 -> IslT m (BasicSet ps n)
intersect = unsafeCoerce go
  where
    go :: BasicSet ps n -> BasicSet ps n -> IslT m (BasicSet ps n)
    go (BasicSet bs1) (BasicSet bs2) = BasicSet <$> BS.intersect bs1 bs2

eliminateLast
  :: forall m ps n. (MonadIO m, KnownNat n, 1 <= n)
  => BasicSet ps n %1 -> IslT m (BasicSet ps (n-1))
eliminateLast = unsafeCoerce go
  where
    go :: BasicSet ps n -> IslT m (BasicSet ps (n-1))
    go (BasicSet bs) =
      let d = fromIntegral $ (natVal $ Proxy @n) - 1
      in BasicSet <$> BS.projectOut bs Isl.islDimSet d 1

fromString :: MonadIO m => String -> IslT m (BasicSet ps n)
fromString str = BasicSet <$> BS.readFromStr str

-- Queries (borrowing)

bsetToString :: BasicSetRef ps n -> String
bsetToString (BasicSetRef bsRef) = BS.toStr bsRef

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
          !bsRef = Isl.BasicSetRef (Isl.unBasicSet rawBs)
      constraints <- unsafeIslFromIO $ \_ -> do
        divExprs <- extractSetDivs bsRef nDims nParams
        BS.foreachConstraint ref $ \c -> do
          result <- extractSetConstraint nParams nDims divExprs c
          evaluate (consume c)
          return result
      return (Ur (PConjunction (Conjunction constraints)), BasicSet rawBs')

-- Resource management

-- | Free a BasicSet within the IslT monad. Sequenced by monadic bind,
-- unlike 'consume' which uses unsafePerformIO and can be deferred.
freeBS :: forall m ps n. MonadIO m => BasicSet ps n %1 -> IslT m ()
freeBS = unsafeCoerce go
  where
    go :: BasicSet ps n -> IslT m ()
    go (BasicSet bs) = freeM bs

-- Consuming combinators

-- | Borrow a BasicSet for a query, then free it. Returns the query result.
consumingBS :: forall m ps n a. MonadIO m => BasicSet ps n %1 -> (BasicSetRef ps n -> a) -> IslT m (Ur a)
consumingBS = unsafeCoerce go
  where
    go :: BasicSet ps n -> (BasicSetRef ps n -> a) -> IslT m (Ur a)
    go (BasicSet bs) f = do
      let !(result, bs') = borrow bs (\ref -> f (BasicSetRef ref))
      freeM bs'
      return (Ur result)

-- | Check if a BasicSet is empty, then free it.
consumingIsEmpty :: forall m ps n. MonadIO m => BasicSet ps n %1 -> IslT m (Ur Bool)
consumingIsEmpty = unsafeCoerce go
  where
    go :: BasicSet ps n -> IslT m (Ur Bool)
    go (BasicSet bs) = do
      let !(r, bs') = borrow bs (\ref -> BS.isEmpty ref)
      freeM bs'
      return (Ur r)
