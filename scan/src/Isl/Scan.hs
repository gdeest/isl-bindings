-- | Polyhedra scanning via the Boulet-Feautrier algorithm.
--
-- This module re-exports the core scanning API. Given a pure
-- constraint representation ('PDisjunction'), build a 'Scanner'
-- and enumerate integer points — all in pure Haskell, with no
-- ISL runtime dependency.
--
-- @
-- import Isl.HighLevel.BasicSet (decomposeBS)
-- import Isl.Scan
--
-- -- Build scanner from a decomposed set:
-- let scanner = mkScanner disjunction
-- -- Enumerate points (with parameter values):
-- let points = scanPoints scanner [10]  -- e.g. N=10
-- @
module Isl.Scan
  ( -- * Building scanners
    mkScanner
  , mkLoopNest
    -- * Nested-loop enumeration
  , scanPoints
  , scanFold
    -- * FSM (state machine) enumeration
  , scanFSM
  , scanFoldFSM
  , ScanState(..)
  , initScan
  , scanStep
    -- * Types
  , Scanner(..)
  , LoopNest(..)
  , LoopLevel(..)
  , AffineBound(..)
    -- * Utilities
  , ceilDiv
  , floorDiv
  ) where

import Isl.Scan.Types
import Isl.Scan.Build
import Isl.Scan.Enumerate
import Isl.Scan.FSM
