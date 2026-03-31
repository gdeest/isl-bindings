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

module Isl.HighLevel.PwMultiAff where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import Data.Proxy
import GHC.TypeLits
import Control.Exception (evaluate)
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.Constraints
import Isl.HighLevel.Map (Map(..))
import Isl.HighLevel.MultiAff (MultiAff(..))
import Isl.HighLevel.Params (KnownSymbols(..), Length)
import qualified Isl.Aff as Aff

import qualified Isl.Types as Isl
import Isl.Types (Borrow(..), Dupable(..), Consumable(..))
import Isl.Monad (IslT(..), Ur(..), unsafeIslFromIO, freeM)
import qualified Isl.PwMultiAff as PMA
import qualified Isl.MultiAff as MA
import qualified Isl.Map as RawMap
import qualified Isl.Set as RawSet
import qualified Isl.BasicSet as RawBS

-- | Owned, parameter- and dimension-indexed PwMultiAff.
-- A piecewise multi-affine function: different multi-affine maps on
-- different pieces of the input domain.
-- Linear — must be consumed exactly once.
newtype PwMultiAff (ps :: [Symbol]) (ni :: Nat) (no :: Nat) = PwMultiAff Isl.PwMultiAff

-- | Borrowed reference to a PwMultiAff. Unrestricted — safe for queries.
newtype PwMultiAffRef (ps :: [Symbol]) (ni :: Nat) (no :: Nat) = PwMultiAffRef Isl.PwMultiAffRef

instance Consumable (PwMultiAff ps ni no) where
  consume = unsafeCoerce $ \(PwMultiAff pma) -> consume pma

instance Dupable (PwMultiAff ps ni no) where
  dup = unsafeCoerce $ \(PwMultiAff pma) ->
    let (a, b) = dup pma in (PwMultiAff a, PwMultiAff b)

instance Borrow (PwMultiAff ps ni no) (PwMultiAffRef ps ni no) where
  borrow = unsafeCoerce $ \(PwMultiAff pma) f ->
    let (result, pma') = borrow pma (\ref -> f (PwMultiAffRef ref))
    in (result, PwMultiAff pma')

-- Construction

fromMultiAff :: forall m ps ni no. MonadIO m
  => MultiAff ps ni no %1 -> IslT m (PwMultiAff ps ni no)
fromMultiAff = unsafeCoerce go
  where
    go :: MultiAff ps ni no -> IslT m (PwMultiAff ps ni no)
    go (MultiAff ma) = PwMultiAff <$> PMA.fromMultiAff ma

fromString :: forall m ps ni no. MonadIO m => String -> IslT m (PwMultiAff ps ni no)
fromString str = PwMultiAff <$> PMA.readFromStr str

-- Operations (consuming)

-- | Convert to a relational map.
toMap :: forall m ps ni no. MonadIO m
  => PwMultiAff ps ni no %1 -> IslT m (Map ps ni no)
toMap = unsafeCoerce go
  where
    go :: PwMultiAff ps ni no -> IslT m (Map ps ni no)
    go (PwMultiAff pma) = Map <$> RawMap.fromPwMultiAff pma

-- Queries

pmaToString :: PwMultiAffRef ps ni no -> String
pmaToString (PwMultiAffRef ref) = PMA.toStr ref

borrowPMA :: forall m ps ni no a. Monad m
  => PwMultiAff ps ni no %1 -> (PwMultiAffRef ps ni no -> a)
  -> IslT m (Ur a, PwMultiAff ps ni no)
borrowPMA = unsafeCoerce go
  where
    go :: PwMultiAff ps ni no -> (PwMultiAffRef ps ni no -> a)
       -> IslT m (Ur a, PwMultiAff ps ni no)
    go (PwMultiAff pma) f =
      let !(result, pma') = borrow pma (\ref -> f (PwMultiAffRef ref))
      in IslT $ \_ -> return (Ur result, PwMultiAff pma')

-- Decomposition

-- | Iterate over the pieces of a PwMultiAff, extracting (domain, output expressions) pairs.
decomposePMA :: forall m ps ni no. (MonadIO m, KnownNat ni, KnownNat no, KnownSymbols ps, KnownNat (Length ps))
  => PwMultiAff ps ni no %1
  -> IslT m (Ur [(Conjunction SetIx, [Expr SetIx])], PwMultiAff ps ni no)
decomposePMA = unsafeCoerce go
  where
    nParams = fromIntegral $ natVal (Proxy @(Length ps))
    nIn     = fromIntegral $ natVal (Proxy @ni)
    nOut    = fromIntegral $ natVal (Proxy @no)

    go :: PwMultiAff ps ni no
       -> IslT m (Ur [(Conjunction SetIx, [Expr SetIx])], PwMultiAff ps ni no)
    go (PwMultiAff rawPMA) = do
      let !(ref, rawPMA') = borrow rawPMA (\r -> r)
          pmaRef = let Isl.PwMultiAff ptr = rawPMA' in Isl.PwMultiAffRef ptr
      pieces <- unsafeIslFromIO $ \_ ->
        PMA.foreachPiece pmaRef $ \domainSet maff -> do
          -- Decompose domain
          let sRef = let Isl.Set ptr = domainSet in Isl.SetRef ptr
          basicSets <- RawSet.foreachBasicSet sRef $ \bs -> do
            let bsRef = let Isl.BasicSet ptr = bs in Isl.BasicSetRef ptr
                rawBsRef = Isl.BasicSetRef (Isl.unBasicSet bs)
            divExprs <- extractSetDivs rawBsRef nIn nParams
            constraints <- RawBS.foreachConstraint bsRef $ \c -> do
              result <- extractSetConstraint nParams nIn divExprs c
              evaluate (consume c)
              return result
            evaluate (consume bs)
            return constraints
          let domainConstrs = Conjunction (concat basicSets)
          -- Extract each output expression from the multi-aff
          let maRef = let Isl.MultiAff ptr = maff in Isl.MultiAffRef ptr
          exprs <- forM [0 .. nOut - 1] $ \j -> do
            aff <- MA.multiAffGetAffCopy maRef (fromIntegral j)
            let affR = let Isl.Aff ptr = aff in Isl.AffRef ptr
            expr <- extractAffExprIO nParams nIn affR
            evaluate (consume aff)
            return expr
          evaluate (consume domainSet)
          evaluate (consume maff)
          return (domainConstrs, exprs)
      return (Ur pieces, PwMultiAff rawPMA')

-- Resource management

freePMA :: forall m ps ni no. MonadIO m => PwMultiAff ps ni no %1 -> IslT m ()
freePMA = unsafeCoerce go
  where
    go :: PwMultiAff ps ni no -> IslT m ()
    go (PwMultiAff pma) = freeM pma

consumingPMA :: forall m ps ni no a. MonadIO m
  => PwMultiAff ps ni no %1 -> (PwMultiAffRef ps ni no -> a) -> IslT m (Ur a)
consumingPMA = unsafeCoerce go
  where
    go :: PwMultiAff ps ni no -> (PwMultiAffRef ps ni no -> a) -> IslT m (Ur a)
    go (PwMultiAff pma) f = do
      let !(result, pma') = borrow pma (\ref -> f (PwMultiAffRef ref))
      freeM pma'
      return (Ur result)
