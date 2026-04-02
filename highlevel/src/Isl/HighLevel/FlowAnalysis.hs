{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Typed wrapper for ISL flow analysis.
--
-- Takes 'NamedMap's (with tuple names for statement/array matching),
-- converts to ISL union maps, runs flow analysis, returns dependencies.
module Isl.HighLevel.FlowAnalysis
  ( computeFlowDeps
  ) where

import Control.Monad.IO.Class (MonadIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.Context (IslT, Ur(..))
import Isl.HighLevel.Pure (NamedMap)
import qualified Isl.HighLevel.UnionMap as UM
import qualified Isl.UnionMap as RawUM
import qualified Isl.FlowAnalysis as Raw


-- | Compute flow dependencies from read/write access relations and a schedule.
--
-- All inputs are 'NamedMap's with proper tuple names (statement names
-- match between accesses and schedule). ISL automatically matches tuples
-- during flow analysis.
--
-- Returns the must-dependence relation as a 'UM.UnionMap'.
computeFlowDeps :: MonadIO m
  => [NamedMap]     -- ^ read access relations (one per stencil offset)
  -> NamedMap       -- ^ write access relation
  -> NamedMap       -- ^ schedule (e.g., identity for unscheduled analysis)
  -> IslT m UM.UnionMap
computeFlowDeps readMaps writeMap schedMap = do
  -- Build reads union
  readUMs <- mapM UM.toUnionMapFromNamed readMaps
  readUM <- foldUM readUMs

  -- Build writes
  writeUM <- UM.toUnionMapFromNamed writeMap

  -- Build schedule
  schedUM <- UM.toUnionMapFromNamed schedMap

  -- Extract raw pointers for FFI
  let !(UM.UnionMap rawReads) = readUM
      !(UM.UnionMap rawWrites) = writeUM
      !(UM.UnionMap rawSched) = schedUM

  -- Compute flow deps
  rawDeps <- Raw.computeFlowDeps rawReads rawWrites rawSched
  return (UM.UnionMap rawDeps)


-- | Fold a list of UnionMaps with union.
foldUM :: forall m. MonadIO m => [UM.UnionMap] -> IslT m UM.UnionMap
foldUM [] = error "foldUM: empty list"
foldUM [x] = return x
foldUM (x:y:rest) = do
  merged <- unionUM x y
  foldUM (merged : rest)

-- | Union two UnionMaps via raw ISL (bypasses linear type restriction).
unionUM :: forall m. MonadIO m => UM.UnionMap -> UM.UnionMap -> IslT m UM.UnionMap
unionUM (UM.UnionMap a) (UM.UnionMap b) = UM.UnionMap <$> RawUM.union a b
