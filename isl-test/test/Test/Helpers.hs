{-# LANGUAGE DataKinds #-}
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
import GHC.TypeLits (Nat, KnownNat)

import Isl.Monad (IslT, Isl, Ur(..), runIslT, runIsl, withCtx)
import Isl.HighLevel.Constraints (Conjunction, MapDim)
import qualified Isl.HighLevel.BasicSet as BS
import qualified Isl.HighLevel.BasicMap as BM
import qualified Isl.HighLevel.Set as Set
import qualified Isl.HighLevel.Map as Map

-- | Run an ISL computation and extract the pure result.
runIslTest :: NFData a => Isl (Ur a) -> a
runIslTest = runIsl

-- | Build a Set from a Conjunction (convenience for tests).
mkSet :: forall n m. (MonadIO m, KnownNat n)
  => Conjunction Integer -> IslT m (Set.Set n)
mkSet conj = do
  bs <- BS.toBasicSet conj
  Set.fromBasicSet bs

-- | Build a Map from a MapDim Conjunction (convenience for tests).
mkMap :: forall ni no m. (MonadIO m, KnownNat ni, KnownNat no)
  => Conjunction MapDim -> IslT m (Map.Map ni no)
mkMap conj = do
  bm <- BM.toBasicMap conj
  Map.fromBasicMap bm
