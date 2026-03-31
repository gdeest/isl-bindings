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

module Isl.HighLevel.PwAff where

import Control.Monad.IO.Class (MonadIO)
import Data.Proxy
import GHC.TypeLits
import Control.Exception (evaluate)
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.Constraints
import Isl.HighLevel.Params (KnownSymbols(..), Length)
import Isl.HighLevel.Set (Set(..))

import qualified Isl.Types as Isl
import Isl.Types (Borrow(..), Dupable(..), Consumable(..))
import Isl.Monad (IslT(..), Ur(..), unsafeIslFromIO, freeM)
import qualified Isl.PwAff as PA
import qualified Isl.Set as RawSet
import qualified Isl.BasicSet as RawBS

-- | Owned, parameter- and dimension-indexed PwAff.
-- A piecewise affine expression: different affine expressions on different
-- pieces of the domain.
-- Linear — must be consumed exactly once.
newtype PwAff (ps :: [Symbol]) (n :: Nat) = PwAff Isl.PwAff

-- | Borrowed reference to a PwAff. Unrestricted — safe for queries.
newtype PwAffRef (ps :: [Symbol]) (n :: Nat) = PwAffRef Isl.PwAffRef

instance Consumable (PwAff ps n) where
  consume = unsafeCoerce $ \(PwAff pa) -> consume pa

instance Dupable (PwAff ps n) where
  dup = unsafeCoerce $ \(PwAff pa) ->
    let (a, b) = dup pa in (PwAff a, PwAff b)

instance Borrow (PwAff ps n) (PwAffRef ps n) where
  borrow = unsafeCoerce $ \(PwAff pa) f ->
    let (result, pa') = borrow pa (\ref -> f (PwAffRef ref))
    in (result, PwAff pa')

-- Construction

fromString :: forall m ps n. MonadIO m => String -> IslT m (PwAff ps n)
fromString str = PwAff <$> PA.readFromStr str

-- Operations (consuming)

unionMin :: forall m ps n. MonadIO m
  => PwAff ps n %1 -> PwAff ps n %1 -> IslT m (PwAff ps n)
unionMin = unsafeCoerce go
  where
    go :: PwAff ps n -> PwAff ps n -> IslT m (PwAff ps n)
    go (PwAff pa1) (PwAff pa2) = PwAff <$> PA.unionMin pa1 pa2

unionMax :: forall m ps n. MonadIO m
  => PwAff ps n %1 -> PwAff ps n %1 -> IslT m (PwAff ps n)
unionMax = unsafeCoerce go
  where
    go :: PwAff ps n -> PwAff ps n -> IslT m (PwAff ps n)
    go (PwAff pa1) (PwAff pa2) = PwAff <$> PA.unionMax pa1 pa2

-- | Get the maximum value of dimension @d@ in a set, as a piecewise affine.
dimMax :: forall m ps n. MonadIO m
  => Set ps n %1 -> Int -> IslT m (PwAff ps n)
dimMax = unsafeCoerce go
  where
    go :: Set ps n -> Int -> IslT m (PwAff ps n)
    go (Set s) d = PwAff <$> RawSet.dimMax s (fromIntegral d)

-- | Get the minimum value of dimension @d@ in a set, as a piecewise affine.
dimMin :: forall m ps n. MonadIO m
  => Set ps n %1 -> Int -> IslT m (PwAff ps n)
dimMin = unsafeCoerce go
  where
    go :: Set ps n -> Int -> IslT m (PwAff ps n)
    go (Set s) d = PwAff <$> RawSet.dimMin s (fromIntegral d)

-- Queries

paToString :: PwAffRef ps n -> String
paToString (PwAffRef ref) = PA.toStr ref

borrowPA :: forall m ps n a. Monad m
  => PwAff ps n %1 -> (PwAffRef ps n -> a)
  -> IslT m (Ur a, PwAff ps n)
borrowPA = unsafeCoerce go
  where
    go :: PwAff ps n -> (PwAffRef ps n -> a)
       -> IslT m (Ur a, PwAff ps n)
    go (PwAff pa) f =
      let !(result, pa') = borrow pa (\ref -> f (PwAffRef ref))
      in IslT $ \_ -> return (Ur result, PwAff pa')

-- Decomposition

-- | Iterate over the pieces of a PwAff, extracting (domain constraints, expression) pairs.
-- Domain is a conjunction of set constraints; expression uses SetDim for set dims.
decomposePwAff :: forall m ps n. (MonadIO m, KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => PwAff ps n %1
  -> IslT m (Ur [(Conjunction SetIx, Expr SetIx)], PwAff ps n)
decomposePwAff = unsafeCoerce go
  where
    nParams = fromIntegral $ natVal (Proxy @(Length ps))
    nDims   = fromIntegral $ natVal (Proxy @n)

    go :: PwAff ps n
       -> IslT m (Ur [(Conjunction SetIx, Expr SetIx)], PwAff ps n)
    go (PwAff rawPA) = do
      let !(ref, rawPA') = borrow rawPA (\r -> r)
          paRef = let Isl.PwAff ptr = rawPA' in Isl.PwAffRef ptr
      pieces <- unsafeIslFromIO $ \_ ->
        PA.foreachPiece paRef $ \domainSet aff -> do
          -- Decompose the domain set into constraints
          -- The domain is a basic set (single conjunction) in most cases,
          -- but to be safe we iterate over basic sets.
          let sRef = let Isl.Set ptr = domainSet in Isl.SetRef ptr
          basicSets <- RawSet.foreachBasicSet sRef $ \bs -> do
            let bsRef = let Isl.BasicSet ptr = bs in Isl.BasicSetRef ptr
                rawBsRef = Isl.BasicSetRef (Isl.unBasicSet bs)
            divExprs <- extractSetDivs rawBsRef nDims nParams
            constraints <- RawBS.foreachConstraint bsRef $ \c -> do
              result <- extractSetConstraint nParams nDims divExprs c
              evaluate (consume c)
              return result
            evaluate (consume bs)
            return constraints
          -- For each piece, there's usually one basic set
          let domainConstrs = Conjunction (concat basicSets)
          -- Extract the affine expression
          let affR = let Isl.Aff ptr = aff in Isl.AffRef ptr
          expr <- extractSetAffExprIO nParams nDims affR
          evaluate (consume domainSet)
          evaluate (consume aff)
          return (domainConstrs, expr)
      return (Ur pieces, PwAff rawPA')

-- Resource management

freePA :: forall m ps n. MonadIO m => PwAff ps n %1 -> IslT m ()
freePA = unsafeCoerce go
  where
    go :: PwAff ps n -> IslT m ()
    go (PwAff pa) = freeM pa

consumingPA :: forall m ps n a. MonadIO m
  => PwAff ps n %1 -> (PwAffRef ps n -> a) -> IslT m (Ur a)
consumingPA = unsafeCoerce go
  where
    go :: PwAff ps n -> (PwAffRef ps n -> a) -> IslT m (Ur a)
    go (PwAff pa) f = do
      let !(result, pa') = borrow pa (\ref -> f (PwAffRef ref))
      freeM pa'
      return (Ur result)
