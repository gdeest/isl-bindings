{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Helpers
  ( runIslTest
  , mkSet
  , mkMap
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (MonadIO)
import GHC.TypeLits (Nat, KnownNat, Symbol)

import Isl.Monad (IslT, Isl, Ur(..), runIslT, runIsl)
import Isl.HighLevel.Constraints (Conjunction, SetIx, MapIx)
import Isl.HighLevel.Params (KnownSymbols, Length)
import qualified Isl.HighLevel.BasicSet as BS
import qualified Isl.HighLevel.BasicMap as BM
import qualified Isl.HighLevel.Set as Set
import qualified Isl.HighLevel.Map as Map

-- | Run an ISL computation and extract the pure result.
runIslTest :: NFData a => Isl (Ur a) -> a
runIslTest = runIsl

-- | Build a Set from a Conjunction (convenience for tests).
mkSet :: forall (ps :: [Symbol]) n m. (MonadIO m, KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => Conjunction SetIx -> IslT m (Set.Set ps n)
mkSet conj = do
  bs <- BS.toBasicSet @ps conj
  Set.fromBasicSet bs

-- | Build a Map from a MapIx Conjunction (convenience for tests).
mkMap :: forall (ps :: [Symbol]) ni no m. (MonadIO m, KnownNat ni, KnownNat no, KnownSymbols ps, KnownNat (Length ps))
  => Conjunction MapIx -> IslT m (Map.Map ps ni no)
mkMap conj = do
  bm <- BM.toBasicMap @ps conj
  Map.fromBasicMap bm
