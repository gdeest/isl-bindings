{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

-- | 2D stencil tiling verification: schedule validity via ISL.
--
-- Demonstrates the core polyhedral insight: tiling a stencil computation
-- requires skewing to make dependencies tile-local. Without skewing,
-- a point at a tile boundary depends on data from a neighboring tile
-- at the previous time step — violating tile-parallel execution.
--
-- Full pipeline:
--   1. Define Jacobi 2D dependencies as ISL maps (constraint DSL)
--   2. Define schedules: identity, skewed, skewed+tiled, tiled-no-skew
--   3. Verify schedule validity: compose deps with schedule, check lex-order
--   4. Execute via MultiScanner, validate against naive
--   5. Generate C code via CKernel, compile, run, benchmark
module Main where

import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.IO (IOUArray)
import Control.Monad (forM_, when)
import System.IO (hFlush, stdout)
import Control.Monad.IO.Class (MonadIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Isl.HighLevel.Context
import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Pure
import Isl.HighLevel.BasicMap (mkBasicMap, BasicMap)
import qualified Isl.HighLevel.Map as MapOp
import qualified Isl.HighLevel.UnionMap as UM
import qualified Isl.HighLevel.UnionSet as US
import qualified Isl.Linear as Isl
import Isl.Scan

import qualified Isl.Space as Space
import qualified Isl.Map as RawM

import Isl.AstBuild
import Isl.Infer.Runtime (compileAndLoad, unloadKernel, CompiledKernel(..))

import Foreign.Marshal.Array (mallocArray)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr)
import Foreign.Storable (pokeElemOff, peekElemOff)
import Foreign.C.Types (CLong(..))


-- =========================================================================
-- Section 1: Multi-Statement Jacobi 2D Dependencies
-- =========================================================================

-- Params: M=0, N=1, T=2 (alphabetical order, ISL convention)
-- Two statements:
--   S0[t,i,j]: new[i][j] = (old[i-1][j] + old[i+1][j] + old[i][j-1] + old[i][j+1]) / 4
--   S1[t,i,j]: old[i][j] = new[i][j]
--
-- Dependencies (named, for ISL tuple matching):
--   { S0[t,i,j] -> S1[t,i,j] }          — compute before copy (same point)
--   { S1[t,i,j] -> S0[t+1,i',j'] }      — copy old[i,j] before next-step reads

-- | Named dep: { S0[t,i,j] -> S1[t,i,j] } — compute before copy
depS0toS1 :: NamedMap
depS0toS1 = NamedMap
  { nmDomainName = Just "S0"
  , nmRangeName  = Just "S1"
  , nmParams     = ["M", "N", "T"]
  , nmNIn        = 3, nmNOut = 3
  , nmConjs      = [mkDep $ \(mp :- np :- tp :- Nil) (t :- i :- j :- Nil) (t' :- i' :- j' :- Nil) ->
      idx t' ==: idx t &&: idx i' ==: idx i &&: idx j' ==: idx j
      &&: idx t >=: cst 1 &&: idx tp >=: idx t
      &&: idx i >=: cst 1 &&: idx np >=: idx i
      &&: idx j >=: cst 1 &&: idx mp >=: idx j]
  }

-- | Named dep: { S1[t,i,j] -> S0[t+1,i-1,j] } — old[i,j] written, read by north neighbor
depS1toS0_N :: NamedMap
depS1toS0_N = NamedMap
  { nmDomainName = Just "S1", nmRangeName = Just "S0"
  , nmParams = ["M","N","T"], nmNIn = 3, nmNOut = 3
  , nmConjs = [mkDep $ \(mp :- np :- tp :- Nil) (t :- i :- j :- Nil) (t' :- i' :- j' :- Nil) ->
      idx t' ==: idx t +: cst 1 &&: idx i' ==: idx i -: cst 1 &&: idx j' ==: idx j
      &&: idx t >=: cst 1 &&: idx tp -: cst 1 >=: idx t
      &&: idx i >=: cst 2 &&: idx np >=: idx i
      &&: idx j >=: cst 1 &&: idx mp >=: idx j]
  }

depS1toS0_S :: NamedMap
depS1toS0_S = NamedMap
  { nmDomainName = Just "S1", nmRangeName = Just "S0"
  , nmParams = ["M","N","T"], nmNIn = 3, nmNOut = 3
  , nmConjs = [mkDep $ \(mp :- np :- tp :- Nil) (t :- i :- j :- Nil) (t' :- i' :- j' :- Nil) ->
      idx t' ==: idx t +: cst 1 &&: idx i' ==: idx i +: cst 1 &&: idx j' ==: idx j
      &&: idx t >=: cst 1 &&: idx tp -: cst 1 >=: idx t
      &&: idx i >=: cst 1 &&: idx np -: cst 1 >=: idx i
      &&: idx j >=: cst 1 &&: idx mp >=: idx j]
  }

depS1toS0_W :: NamedMap
depS1toS0_W = NamedMap
  { nmDomainName = Just "S1", nmRangeName = Just "S0"
  , nmParams = ["M","N","T"], nmNIn = 3, nmNOut = 3
  , nmConjs = [mkDep $ \(mp :- np :- tp :- Nil) (t :- i :- j :- Nil) (t' :- i' :- j' :- Nil) ->
      idx t' ==: idx t +: cst 1 &&: idx i' ==: idx i &&: idx j' ==: idx j -: cst 1
      &&: idx t >=: cst 1 &&: idx tp -: cst 1 >=: idx t
      &&: idx i >=: cst 1 &&: idx np >=: idx i
      &&: idx j >=: cst 2 &&: idx mp >=: idx j]
  }

depS1toS0_E :: NamedMap
depS1toS0_E = NamedMap
  { nmDomainName = Just "S1", nmRangeName = Just "S0"
  , nmParams = ["M","N","T"], nmNIn = 3, nmNOut = 3
  , nmConjs = [mkDep $ \(mp :- np :- tp :- Nil) (t :- i :- j :- Nil) (t' :- i' :- j' :- Nil) ->
      idx t' ==: idx t +: cst 1 &&: idx i' ==: idx i &&: idx j' ==: idx j +: cst 1
      &&: idx t >=: cst 1 &&: idx tp -: cst 1 >=: idx t
      &&: idx i >=: cst 1 &&: idx np >=: idx i
      &&: idx j >=: cst 1 &&: idx mp -: cst 1 >=: idx j]
  }

-- Anti-dep on new array: S1 reads new[i][j] before S0 at t+1 overwrites it
depS1toS0_self :: NamedMap
depS1toS0_self = NamedMap
  { nmDomainName = Just "S1", nmRangeName = Just "S0"
  , nmParams = ["M","N","T"], nmNIn = 3, nmNOut = 3
  , nmConjs = [mkDep $ \(mp :- np :- tp :- Nil) (t :- i :- j :- Nil) (t' :- i' :- j' :- Nil) ->
      idx t' ==: idx t +: cst 1 &&: idx i' ==: idx i &&: idx j' ==: idx j
      &&: idx t >=: cst 1 &&: idx tp -: cst 1 >=: idx t
      &&: idx i >=: cst 1 &&: idx np >=: idx i
      &&: idx j >=: cst 1 &&: idx mp >=: idx j]
  }

-- Anti-deps on old: S0 reads old[i±1,j±1] before S1 writes old[i±1,j±1] at same t
-- { S0[t,i,j] -> S1[t,i-1,j] }  — S0 reads old[i-1][j], S1 writes it
depAnti_N :: NamedMap
depAnti_N = NamedMap
  { nmDomainName = Just "S0", nmRangeName = Just "S1"
  , nmParams = ["M","N","T"], nmNIn = 3, nmNOut = 3
  , nmConjs = [mkDep $ \(mp :- np :- tp :- Nil) (t :- i :- j :- Nil) (t' :- i' :- j' :- Nil) ->
      idx t' ==: idx t &&: idx i' ==: idx i -: cst 1 &&: idx j' ==: idx j
      &&: idx t >=: cst 1 &&: idx tp >=: idx t
      &&: idx i >=: cst 2 &&: idx np >=: idx i
      &&: idx j >=: cst 1 &&: idx mp >=: idx j]
  }

depAnti_S :: NamedMap
depAnti_S = NamedMap
  { nmDomainName = Just "S0", nmRangeName = Just "S1"
  , nmParams = ["M","N","T"], nmNIn = 3, nmNOut = 3
  , nmConjs = [mkDep $ \(mp :- np :- tp :- Nil) (t :- i :- j :- Nil) (t' :- i' :- j' :- Nil) ->
      idx t' ==: idx t &&: idx i' ==: idx i +: cst 1 &&: idx j' ==: idx j
      &&: idx t >=: cst 1 &&: idx tp >=: idx t
      &&: idx i >=: cst 1 &&: idx np -: cst 1 >=: idx i
      &&: idx j >=: cst 1 &&: idx mp >=: idx j]
  }

depAnti_W :: NamedMap
depAnti_W = NamedMap
  { nmDomainName = Just "S0", nmRangeName = Just "S1"
  , nmParams = ["M","N","T"], nmNIn = 3, nmNOut = 3
  , nmConjs = [mkDep $ \(mp :- np :- tp :- Nil) (t :- i :- j :- Nil) (t' :- i' :- j' :- Nil) ->
      idx t' ==: idx t &&: idx i' ==: idx i &&: idx j' ==: idx j -: cst 1
      &&: idx t >=: cst 1 &&: idx tp >=: idx t
      &&: idx i >=: cst 1 &&: idx np >=: idx i
      &&: idx j >=: cst 2 &&: idx mp >=: idx j]
  }

depAnti_E :: NamedMap
depAnti_E = NamedMap
  { nmDomainName = Just "S0", nmRangeName = Just "S1"
  , nmParams = ["M","N","T"], nmNIn = 3, nmNOut = 3
  , nmConjs = [mkDep $ \(mp :- np :- tp :- Nil) (t :- i :- j :- Nil) (t' :- i' :- j' :- Nil) ->
      idx t' ==: idx t &&: idx i' ==: idx i &&: idx j' ==: idx j +: cst 1
      &&: idx t >=: cst 1 &&: idx tp >=: idx t
      &&: idx i >=: cst 1 &&: idx np >=: idx i
      &&: idx j >=: cst 1 &&: idx mp -: cst 1 >=: idx j]
  }


-- Helper: build a map conjunction from the constraint DSL
mkDep :: (IxList 3 MapIx -> IxList 3 MapIx -> IxList 3 MapIx -> Conjunction MapIx)
      -> Conjunction MapIx
mkDep f = let PMapConjunction c = mkPMapConjunction @'["M","N","T"] @3 @3 f in c

-- | All Jacobi deps (flow + output + anti) as named union map.
mkJacobiNamedDeps :: MonadIO m => IslT m UM.UnionMap
mkJacobiNamedDeps = do
  -- Flow deps: S0→S1 (compute before copy) + S1→S0 (copy before next-step read)
  u0 <- UM.toUnionMapFromNamed depS0toS1
  u1 <- UM.toUnionMapFromNamed depS1toS0_N
  u01 <- UM.union u0 u1
  u2 <- UM.toUnionMapFromNamed depS1toS0_S
  u012 <- UM.union u01 u2
  u3 <- UM.toUnionMapFromNamed depS1toS0_W
  u0123 <- UM.union u012 u3
  u4 <- UM.toUnionMapFromNamed depS1toS0_E
  flow <- UM.union u0123 u4
  -- Anti-dep on new array
  selfDep <- UM.toUnionMapFromNamed depS1toS0_self
  flow2 <- UM.union flow selfDep
  -- Anti-deps on old: S0 reads before S1 writes at same t
  a1 <- UM.toUnionMapFromNamed depAnti_N
  a2 <- UM.toUnionMapFromNamed depAnti_S
  a12 <- UM.union a1 a2
  a3 <- UM.toUnionMapFromNamed depAnti_W
  a123 <- UM.union a12 a3
  a4 <- UM.toUnionMapFromNamed depAnti_E
  anti <- UM.union a123 a4
  UM.union flow2 anti


-- =========================================================================
-- Section 2: Gauss-Seidel Dependencies
-- =========================================================================

-- | { [t,i,j] -> [t,i+1,j] } — south within same time step
mkGsSouth :: MonadIO m => IslT m (BasicMap '["M","N","T"] 3 3)
mkGsSouth = mkBasicMap @'["M","N","T"] @3 @3 $
  \(mp :- np :- tp :- Nil) (t :- i :- j :- Nil) (t' :- i' :- j' :- Nil) ->
    idx t' ==: idx t
    &&: idx i' ==: idx i +: cst 1
    &&: idx j' ==: idx j
    &&: idx t >=: cst 1 &&: idx tp >=: idx t
    &&: idx i >=: cst 1 &&: idx np -: cst 1 >=: idx i
    &&: idx j >=: cst 1 &&: idx mp >=: idx j

-- | { [t,i,j] -> [t,i,j+1] } — east within same time step
mkGsEast :: MonadIO m => IslT m (BasicMap '["M","N","T"] 3 3)
mkGsEast = mkBasicMap @'["M","N","T"] @3 @3 $
  \(mp :- np :- tp :- Nil) (t :- i :- j :- Nil) (t' :- i' :- j' :- Nil) ->
    idx t' ==: idx t
    &&: idx i' ==: idx i
    &&: idx j' ==: idx j +: cst 1
    &&: idx t >=: cst 1 &&: idx tp >=: idx t
    &&: idx i >=: cst 1 &&: idx np >=: idx i
    &&: idx j >=: cst 1 &&: idx mp -: cst 1 >=: idx j

-- | { [t,i,j] -> [t+1,i,j] } — self-dependence across time
mkGsTime :: MonadIO m => IslT m (BasicMap '["M","N","T"] 3 3)
mkGsTime = mkBasicMap @'["M","N","T"] @3 @3 $
  \(mp :- np :- tp :- Nil) (t :- i :- j :- Nil) (t' :- i' :- j' :- Nil) ->
    idx t' ==: idx t +: cst 1
    &&: idx i' ==: idx i
    &&: idx j' ==: idx j
    &&: idx t >=: cst 1 &&: idx tp -: cst 1 >=: idx t
    &&: idx i >=: cst 1 &&: idx np >=: idx i
    &&: idx j >=: cst 1 &&: idx mp >=: idx j

mkGaussSeidelDeps :: MonadIO m => IslT m UM.UnionMap
mkGaussSeidelDeps = Isl.do
  s <- mkGsSouth
  s' <- MapOp.fromBasicMap s
  us <- UM.fromMap s'
  e <- mkGsEast
  e' <- MapOp.fromBasicMap e
  ue <- UM.fromMap e'
  use' <- UM.union us ue
  t <- mkGsTime
  t' <- MapOp.fromBasicMap t
  ut <- UM.fromMap t'
  UM.union use' ut


-- =========================================================================
-- Section 3: Schedule Maps (constraint DSL)
-- =========================================================================

-- | Identity: { [t,i,j] -> [t, i, j] }
mkIdentitySched :: MonadIO m => IslT m (BasicMap '["M","N","T"] 3 3)
mkIdentitySched = mkBasicMap @'["M","N","T"] @3 @3 $
  \_ (t :- i :- j :- Nil) (s0 :- s1 :- s2 :- Nil) ->
    idx s0 ==: idx t
    &&: idx s1 ==: idx i
    &&: idx s2 ==: idx j

-- | Skewed: { [t,i,j] -> [t, t+i, t+j] }
mkSkewedSched :: MonadIO m => IslT m (BasicMap '["M","N","T"] 3 3)
mkSkewedSched = mkBasicMap @'["M","N","T"] @3 @3 $
  \_ (t :- i :- j :- Nil) (s0 :- s1 :- s2 :- Nil) ->
    idx s0 ==: idx t
    &&: idx s1 ==: idx t +: idx i
    &&: idx s2 ==: idx t +: idx j

-- | Skewed+Tiled: { [t,i,j] -> [floor(t/4), floor((t+i)/32), floor((t+j)/32), t, t+i, t+j] }
mkTiledSkewSched :: MonadIO m => IslT m (BasicMap '["M","N","T"] 3 6)
mkTiledSkewSched = mkBasicMap @'["M","N","T"] @3 @6 $
  \_ (t :- i :- j :- Nil) (tt :- ti :- tj :- t2 :- ip :- jp :- Nil) ->
    idx tt ==: FloorDiv (idx t) 4
    &&: idx ti ==: FloorDiv (idx t +: idx i) 32
    &&: idx tj ==: FloorDiv (idx t +: idx j) 32
    &&: idx t2 ==: idx t
    &&: idx ip ==: idx t +: idx i
    &&: idx jp ==: idx t +: idx j

-- | Tiled WITHOUT skew: { [t,i,j] -> [floor(t/4), floor(i/32), floor(j/32), t, i, j] }
-- This schedule is INVALID for the Jacobi stencil — deps cross tile boundaries.
mkTiledNoSkewSched :: MonadIO m => IslT m (BasicMap '["M","N","T"] 3 6)
mkTiledNoSkewSched = mkBasicMap @'["M","N","T"] @3 @6 $
  \_ (t :- i :- j :- Nil) (tt :- ti :- tj :- t2 :- ip :- jp :- Nil) ->
    idx tt ==: FloorDiv (idx t) 4
    &&: idx ti ==: FloorDiv (idx i) 32
    &&: idx tj ==: FloorDiv (idx j) 32
    &&: idx t2 ==: idx t
    &&: idx ip ==: idx i
    &&: idx jp ==: idx j


-- =========================================================================
-- Section 4: Schedule Validity Checker
-- =========================================================================

-- | Build the lexicographically-greater-or-equal order { [a] -> [b] : a >=_lex b }
-- using ISL's built-in isl_map_lex_ge primitive.
buildLexGe :: MonadIO m => Int -> IslT m UM.UnionMap
buildLexGe n = do
  sp <- Space.setAlloc 0 n    -- set space: lexGe builds { [a] -> [b] : a >=_lex b }
  rawMap <- RawM.lexGe sp
  rawUm <- RawM.toUnionMap rawMap
  Isl.pure (UM.UnionMap rawUm)

-- | Check whether a schedule respects all data dependencies.
--
-- A schedule θ is valid iff for every dependence (src → dst),
-- θ(src) <_lex θ(dst) — the source executes strictly before the target.
--
-- Algorithm:
--   1. Compose: schedDep = θ ∘ dep ∘ θ⁻¹   (deps in schedule space)
--   2. violations = schedDep ∩ lexGe         (where source ≥_lex target)
--   3. valid ⟺ violations = ∅
checkValidity :: MonadIO m
  => UM.UnionMap %1  -- ^ all dependencies (consumed)
  -> UM.UnionMap %1  -- ^ schedule map (consumed — for applyRange)
  -> UM.UnionMap %1  -- ^ schedule map copy (consumed — for reverse+applyDomain)
  -> Int             -- ^ number of schedule output dimensions
  -> IslT m (Ur (Bool, String))
checkValidity deps sched schedCopy nOut = Isl.do
  -- Step 1: { [src] -> [θ(dst)] }
  step1 <- UM.applyRange deps sched
  -- Step 2: applyDomain replaces src with θ(src) → { [θ(src)] -> [θ(dst)] }
  schedDep <- UM.applyDomain step1 schedCopy
  -- Step 3: build { [a] -> [b] : a >=_lex b }
  lexGe <- buildLexGe nOut
  -- Step 4: find violations
  violations <- UM.intersect schedDep lexGe
  (Ur empty, violations') <- UM.isEmpty violations
  -- Step 5: diagnostic string
  (Ur vStr, violations'') <- UM.borrowUM violations' UM.umapToString
  UM.freeUnionMap violations''
  Isl.pure (Ur (empty, vStr))


-- =========================================================================
-- Section 5: Named Domains & Schedules (for MultiScanner execution)
-- =========================================================================

-- Two statements: S0 (compute new from old), S1 (copy new back to old)
-- Skewed schedule: S0[t,i,j] -> [t, t+i, t+j, 0]
--                  S1[t,i,j] -> [t, t+i, t+j, 1]

s0Domain :: NamedSet
s0Domain = mkNamedPConjunction @"S0" @'["M","N","T"] @3 $
  \(mp :- np :- tp :- Nil) (t :- i :- j :- Nil) ->
    idx t >=: cst 1 &&: idx t <=: idx tp
    &&: idx i >=: cst 1 &&: idx i <=: idx np
    &&: idx j >=: cst 1 &&: idx j <=: idx mp

s1Domain :: NamedSet
s1Domain = mkNamedPConjunction @"S1" @'["M","N","T"] @3 $
  \(mp :- np :- tp :- Nil) (t :- i :- j :- Nil) ->
    idx t >=: cst 1 &&: idx t <=: idx tp
    &&: idx i >=: cst 1 &&: idx i <=: idx np
    &&: idx j >=: cst 1 &&: idx j <=: idx mp

-- Identity schedule (phase-separated): S0[t,i,j] -> [t, 0, i, j]; S1 -> [t, 1, i, j]
s0SchedIdentity :: NamedMap
s0SchedIdentity = mkNamedPMapConjunction @"S0" @'["M","N","T"] @3 @4 $
  \_ (t :- i :- j :- Nil) (d0 :- d1 :- d2 :- d3 :- Nil) ->
    idx d0 ==: idx t
    &&: idx d1 ==: cst 0
    &&: idx d2 ==: idx i
    &&: idx d3 ==: idx j

s1SchedIdentity :: NamedMap
s1SchedIdentity = mkNamedPMapConjunction @"S1" @'["M","N","T"] @3 @4 $
  \_ (t :- i :- j :- Nil) (d0 :- d1 :- d2 :- d3 :- Nil) ->
    idx d0 ==: idx t
    &&: idx d1 ==: cst 1
    &&: idx d2 ==: idx i
    &&: idx d3 ==: idx j

-- Phase-separated skewed schedule: all S0s (compute) before S1s (copy) per time step.
-- S0[t,i,j] -> [t, 0, t+i, t+j]
-- S1[t,i,j] -> [t, 1, t+i, t+j]
s0SchedSkewed :: NamedMap
s0SchedSkewed = mkNamedPMapConjunction @"S0" @'["M","N","T"] @3 @4 $
  \_ (t :- i :- j :- Nil) (d0 :- d1 :- d2 :- d3 :- Nil) ->
    idx d0 ==: idx t
    &&: idx d1 ==: cst 0
    &&: idx d2 ==: idx t +: idx i
    &&: idx d3 ==: idx t +: idx j

s1SchedSkewed :: NamedMap
s1SchedSkewed = mkNamedPMapConjunction @"S1" @'["M","N","T"] @3 @4 $
  \_ (t :- i :- j :- Nil) (d0 :- d1 :- d2 :- d3 :- Nil) ->
    idx d0 ==: idx t
    &&: idx d1 ==: cst 1
    &&: idx d2 ==: idx t +: idx i
    &&: idx d3 ==: idx t +: idx j


-- Tiled+skewed schedule: tile the spatial (skewed) dims.
-- Phase separation INSIDE the time loop (stmt after t, before spatial):
-- S0[t,i,j] -> [floor(t/4), floor((t+i)/32), floor((t+j)/32), t, 0, t+i, t+j]
-- S1[t,i,j] -> [floor(t/4), floor((t+i)/32), floor((t+j)/32), t, 1, t+i, t+j]
s0SchedTiled :: NamedMap
s0SchedTiled = mkNamedPMapConjunction @"S0" @'["M","N","T"] @3 @7 $
  \_ (t :- i :- j :- Nil) (tt :- ti :- tj :- d3 :- stmt :- d5 :- d6 :- Nil) ->
    idx tt ==: FloorDiv (idx t) 4
    &&: idx ti ==: FloorDiv (idx t +: idx i) 32
    &&: idx tj ==: FloorDiv (idx t +: idx j) 32
    &&: idx d3 ==: idx t
    &&: idx stmt ==: cst 0
    &&: idx d5 ==: idx t +: idx i
    &&: idx d6 ==: idx t +: idx j

s1SchedTiled :: NamedMap
s1SchedTiled = mkNamedPMapConjunction @"S1" @'["M","N","T"] @3 @7 $
  \_ (t :- i :- j :- Nil) (tt :- ti :- tj :- d3 :- stmt :- d5 :- d6 :- Nil) ->
    idx tt ==: FloorDiv (idx t) 4
    &&: idx ti ==: FloorDiv (idx t +: idx i) 32
    &&: idx tj ==: FloorDiv (idx t +: idx j) 32
    &&: idx d3 ==: idx t
    &&: idx stmt ==: cst 1
    &&: idx d5 ==: idx t +: idx i
    &&: idx d6 ==: idx t +: idx j


-- =========================================================================
-- Section 6: ISL AST-Based C Code Generation
-- =========================================================================

-- | Wrap an ISL-generated C skeleton in a complete compilable function.
-- Statement bodies are defined as C macros. Includes ISL's runtime macros
-- (floord, min, max) needed by the generated loop bounds.
wrapKernel :: String -> [(String, String)] -> String -> String
wrapKernel funcName stmtMacros cSkeleton =
  let stmtDefs = unlines
        [ "#define " ++ name ++ "(_t_, _i_, _j_) do { " ++ body ++ " } while(0)"
        | (name, body) <- stmtMacros
        ]
      header = unlines
        [ "#include <stdint.h>"
        , "#include <math.h>"
        , ""
        , "// ISL runtime macros"
        , "#define floord(n,d) (((n)<0) ? -((-(n)+(d)-1)/(d)) : (n)/(d))"
        , "#define min(x,y)    (((x)<(y)) ? (x) : (y))"
        , "#define max(x,y)    (((x)>(y)) ? (x) : (y))"
        , ""
        , stmtDefs
        , ""
        , "void " ++ funcName ++ "(double* restrict old_arr, double* restrict new_arr,"
        , "                    int64_t N, int64_t M, int64_t T, int64_t stride) {"
        ]
      footer = unlines
        [ "}"
        , ""
        , "#undef floord"
        , "#undef min"
        , "#undef max"
        ]
      indented = unlines ["    " ++ l | l <- lines cSkeleton, not (null l)]
  in header ++ indented ++ footer

-- FFI types for the C kernels
type JacobiKernelC = Ptr Double -> Ptr Double -> CLong -> CLong -> CLong -> CLong -> IO ()
foreign import ccall "dynamic"
  mkJacobiKernel :: FunPtr JacobiKernelC -> JacobiKernelC


-- =========================================================================
-- Main
-- =========================================================================

main :: IO ()
main = do
  putStrLn "=== 2D Stencil Tiling Verification ==="
  putStrLn ""

  -- ── Schedule validity checks (multi-statement) ──────────────────────────
  putStrLn "── Jacobi 2D multi-statement dependencies ──"
  putStrLn "  { S0[t,i,j] -> S1[t,i,j] }          — compute before copy"
  putStrLn "  { S1[t,i,j] -> S0[t+1,i±1,j±1] }    — copy before next-step reads"
  putStrLn ""

  -- | Check a named schedule against named deps.
  -- The schedule is a pair of NamedMaps (S0, S1).
  -- Both deps and schedules use ISL tuple name matching.
  let checkNamedSched label s0sched s1sched = do
        (valid, vStr) <- runIslT $ Isl.do
          deps <- mkJacobiNamedDeps
          sched1 <- UM.toUnionMapFromNamed s0sched
          s1um <- UM.toUnionMapFromNamed s1sched
          schedA <- UM.union sched1 s1um
          -- Need a copy of the schedule for applyDomain
          sched2 <- UM.toUnionMapFromNamed s0sched
          s1um2 <- UM.toUnionMapFromNamed s1sched
          schedB <- UM.union sched2 s1um2
          -- Determine nOut from the schedule
          let nOut = nmNOut s0sched
          checkValidity deps schedA schedB nOut
        putStrLn $ "  " ++ label ++ ":"
        putStrLn $ "    " ++ (if valid then "VALID ✓" else "INVALID ✗")
        when (not valid) $
          putStrLn $ "    violations: " ++ take 200 vStr ++ "..."

  checkNamedSched "identity [t, 0/1, i, j]"
    s0SchedIdentity s1SchedIdentity
  checkNamedSched "skewed [t, 0/1, t+i, t+j]"
    s0SchedSkewed s1SchedSkewed
  checkNamedSched "tiled+skewed (EXPECT INVALID: anti-deps cross tile boundaries)"
    s0SchedTiled s1SchedTiled
  putStrLn ""

  -- ── Execution via MultiScanner ─────────────────────────────────────────
  let nRows = 5 :: Int
      mCols = 6 :: Int
      tSteps = 3 :: Int

  putStrLn $ "── Execution: " ++ show nRows ++ "×" ++ show mCols
           ++ " grid, " ++ show tSteps ++ " time steps ──"

  old <- newArray ((0,0),(nRows+1,mCols+1)) 0.0 :: IO (IOUArray (Int,Int) Double)
  new <- newArray ((0,0),(nRows+1,mCols+1)) 0.0 :: IO (IOUArray (Int,Int) Double)
  -- Interior starts at 1.0
  forM_ [1..nRows] $ \i ->
    forM_ [1..mCols] $ \j -> do
      writeArray old (i,j) 1.0
      writeArray new (i,j) 1.0

  -- Build multi-scanner with skewed schedule
  let (nsets, nmaps) = runIsl $ Isl.do
        s0us <- US.toUnionSetFromNamed s0Domain
        s0um <- UM.toUnionMapFromNamed s0SchedSkewed
        s0time <- UM.applyToSet s0us s0um
        (Ur ns0, s0t') <- US.decomposeUnionSetNamed s0time
        US.freeUnionSet s0t'

        s1us <- US.toUnionSetFromNamed s1Domain
        s1um <- UM.toUnionMapFromNamed s1SchedSkewed
        s1time <- UM.applyToSet s1us s1um
        (Ur ns1, s1t') <- US.decomposeUnionSetNamed s1time
        US.freeUnionSet s1t'

        s0um2 <- UM.toUnionMapFromNamed s0SchedSkewed
        s0inv <- UM.reverse s0um2
        (Ur nm0, s0i') <- UM.decomposeUnionMapNamed s0inv
        UM.freeUnionMap s0i'

        s1um2 <- UM.toUnionMapFromNamed s1SchedSkewed
        s1inv <- UM.reverse s1um2
        (Ur nm1, s1i') <- UM.decomposeUnionMapNamed s1inv
        UM.freeUnionMap s1i'

        let tag name nsl = [ns { nsName = Just name } | ns <- nsl]
        Isl.pure (Ur (tag "S0" ns0 ++ tag "S1" ns1, nm0 ++ nm1))

  let ms = mkMultiScannerFromNamed @3 nsets nmaps

  putStrLn "  Scanner (skewed schedule):"
  putStrLn $ prettyMultiScanner ["d0", "d1", "d2", "stmt"] ms

  -- Params: M=0, N=1, T=2 (alphabetical)
  let params = mkVec @3 [fromIntegral mCols, fromIntegral nRows, fromIntegral tSteps]

  -- Execute via scanner
  scanMultiForM_ ms params $ \pt -> do
    let [_t, i, j] = map fromIntegral (spOrigCoord pt)
    case spStmt pt of
      "S0" -> do
        a <- readArray old (i-1, j)
        b <- readArray old (i+1, j)
        c <- readArray old (i, j-1)
        d <- readArray old (i, j+1)
        writeArray new (i, j) ((a + b + c + d) / 4.0)
      "S1" -> do
        v <- readArray new (i, j)
        writeArray old (i, j) v
      _ -> return ()

  -- ── Naive verification ──────────────────────────────────────────────────
  naiveOld <- newArray ((0,0),(nRows+1,mCols+1)) 0.0 :: IO (IOUArray (Int,Int) Double)
  naiveNew <- newArray ((0,0),(nRows+1,mCols+1)) 0.0 :: IO (IOUArray (Int,Int) Double)
  forM_ [1..nRows] $ \i ->
    forM_ [1..mCols] $ \j -> do
      writeArray naiveOld (i,j) 1.0
      writeArray naiveNew (i,j) 1.0

  forM_ [1..tSteps] $ \_ -> do
    forM_ [1..nRows] $ \i -> forM_ [1..mCols] $ \j -> do
      a <- readArray naiveOld (i-1, j)
      b <- readArray naiveOld (i+1, j)
      c <- readArray naiveOld (i, j-1)
      d <- readArray naiveOld (i, j+1)
      writeArray naiveNew (i, j) ((a + b + c + d) / 4.0)
    forM_ [1..nRows] $ \i -> forM_ [1..mCols] $ \j -> do
      v <- readArray naiveNew (i, j)
      writeArray naiveOld (i, j) v

  mismatch <- or <$> sequence
    [ do a <- readArray old (i,j); b <- readArray naiveOld (i,j)
         return (roundTo 8 a /= roundTo 8 b)
    | i <- [1..nRows], j <- [1..mCols]]
  putStrLn $ "  Validation: " ++ if mismatch then "MISMATCH!" else "matches naive implementation ✓"

  let nPts = length (scanMulti ms params)
  putStrLn $ "  (" ++ show nPts ++ " total scan points)"
  putStrLn ""

  -- ── ISL AST-Based C Code Generation ──────────────────────────────────────
  -- The SAME schedule used for validity checking now drives code generation.
  -- ISL's ast_build produces the loop structure; we supply statement macros.

  -- Statement macros: ISL calls S0(t, i, j) with the original domain coords.
  -- The macro parameters are positional: first=t, second=i, third=j.
  -- Statement macros. MUST parenthesize args because ISL passes expressions like (-c0 + c2).
  let stmtMacros =
        [ ("S0", "new_arr[(_i_) * stride + (_j_)] = "
                 ++ "(old_arr[((_i_)-1) * stride + (_j_)] + old_arr[((_i_)+1) * stride + (_j_)] "
                 ++ "+ old_arr[(_i_) * stride + ((_j_)-1)] + old_arr[(_i_) * stride + ((_j_)+1)]) * 0.25;")
        , ("S1", "old_arr[(_i_) * stride + (_j_)] = new_arr[(_i_) * stride + (_j_)];")
        ]

  hFlush stdout
  putStrLn "── Starting ISL AST codegen ──"
  hFlush stdout

  -- | Build schedule UnionMap from NamedMaps, intersect with domain,
  -- generate C via ISL AST builder. All in one IslT block.
  let islCodegen s0sched s1sched = runIslT $ do
        -- Build schedule union map
        s0um <- UM.toUnionMapFromNamed s0sched
        s1um <- UM.toUnionMapFromNamed s1sched
        schedMap <- UM.union s0um s1um
        -- Intersect with iteration domain
        s0us <- US.toUnionSetFromNamed s0Domain
        s1us <- US.toUnionSetFromNamed s1Domain
        domainUS <- US.union s0us s1us
        schedDom <- UM.intersectDomain schedMap domainUS
        -- Print the schedule map for display
        (Ur schedStr, schedDom') <- UM.borrowUM schedDom UM.umapToString
        -- Extract raw UnionMap pointer and pass to AST builder.
        -- schedDom' is consumed by pattern match; ISL takes ownership via ast_build.
        let !(UM.UnionMap raw) = schedDom'
        build <- astBuildAlloc
        node <- astBuildNodeFromScheduleMap build raw
        cCode <- astNodeToC node
        astNodeFree node
        pure (Ur (schedStr, cCode))

  let genAndPrint label funcName s0sched s1sched = do
        (schedStr, cSkel) <- islCodegen s0sched s1sched
        putStrLn $ "── ISL AST codegen: " ++ label ++ " ──"
        putStrLn $ "  schedule: " ++ schedStr
        let src = wrapKernel funcName stmtMacros cSkel
        putStrLn src
        return src

  naiveSrc <- genAndPrint "naive (identity)" "jacobi2d_naive"
                s0SchedIdentity s1SchedIdentity
  skewedSrc <- genAndPrint "skewed" "jacobi2d_skewed"
                 s0SchedSkewed s1SchedSkewed
  -- NOTE: tiled+skewed schedule is INVALID for double-buffered Jacobi
  -- (anti-deps cross tile boundaries). ISL correctly detects this above.
  -- Only naive + skewed kernels are compiled and benchmarked.
  hFlush stdout

  -- ── Compile & Benchmark ────────────────────────────────────────────────
  let benchN = 500 :: Int
      benchM = 500 :: Int
      benchT = 50 :: Int
      stride = benchM + 2
      arrSize = (benchN + 2) * (benchM + 2)

  putStrLn $ "── Benchmark: " ++ show benchN ++ "×" ++ show benchM
           ++ " grid, " ++ show benchT ++ " steps ──"

  hFlush stdout
  putStrLn "  Compiling naive C kernel (ISL-generated)..."
  naiveCK <- compileAndLoad "jacobi2d_naive" naiveSrc
  putStrLn "  Compiling skewed C kernel (ISL-generated)..."
  skewedCK <- compileAndLoad "jacobi2d_skewed" skewedSrc
  putStrLn "  All compiled." >> hFlush stdout

  let sampleIdxs = [i * stride + j | i <- [1, 15, 25, 31, 32, 33, benchN `div` 2, benchN]
                                    , j <- [1, 15, 25, 31, 32, 33, benchM `div` 2, benchM]]

  let initArr p = do
        forM_ [0..arrSize-1] $ \k -> pokeElemOff p k (0.0 :: Double)
        -- Non-uniform initial condition for debugging
        forM_ [1..benchN] $ \i -> forM_ [1..benchM] $ \j ->
          pokeElemOff p (i * stride + j) (fromIntegral (i + j) :: Double)

  let runKernel ck = do
        pOld <- mallocArray arrSize :: IO (Ptr Double)
        pNew <- mallocArray arrSize :: IO (Ptr Double)
        initArr pOld; initArr pNew
        t0 <- getCurrentTime
        mkJacobiKernel (castFunPtr $ ckFuncPtr ck)
          pOld pNew
          (fromIntegral benchN) (fromIntegral benchM)
          (fromIntegral benchT) (fromIntegral stride)
        t1 <- getCurrentTime
        let elapsed = realToFrac (diffUTCTime t1 t0) :: Double
        -- Read a few sample values for validation
        samples <- mapM (peekElemOff pOld) sampleIdxs
        free pOld; free pNew
        return (elapsed, samples)

  (naiveTime, naiveRes) <- runKernel naiveCK
  (skewedTime, skewedRes) <- runKernel skewedCK

  putStrLn $ "  Naive C (ISL):   " ++ show (roundTo 4 naiveTime) ++ " s"
  putStrLn $ "  Skewed C (ISL):  " ++ show (roundTo 4 skewedTime) ++ " s"
  when (naiveTime > 0) $
    putStrLn $ "  Speedup:         " ++ show (roundTo 2 (naiveTime / skewedTime)) ++ "x"

  let valMismatch = any (\(a, b) -> roundTo 6 a /= roundTo 6 b) (zip naiveRes skewedRes)
  putStrLn $ "  Validation:      " ++ if valMismatch then "MISMATCH!" else "OK ✓"

  unloadKernel naiveCK; unloadKernel skewedCK


-- =========================================================================
-- Utilities
-- =========================================================================

roundTo :: Int -> Double -> Double
roundTo n x = fromIntegral (round (x * 10^n) :: Integer) / 10^n
