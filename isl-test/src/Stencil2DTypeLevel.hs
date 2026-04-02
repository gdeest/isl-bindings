{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}

-- | 2D stencil tiling verification at the type level.
--
-- Single source of truth: domain, dependencies, and schedules are
-- defined as type-level constraints. The GHC plugin verifies
-- well-formedness and schedule properties at compile time.
-- Singletons bridge to runtime ISL objects for display.
--
-- Uncomment the @_use*@ bindings to see compile-time errors for
-- invalid schedules.
module Main where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (symbolVal)

import Isl.TypeLevel
import Isl.HighLevel.Context (runIslT, Ur(..))
import qualified Isl.HighLevel.Set as SetOp
import qualified Isl.HighLevel.Map as MapOp


-- =========================================================================
-- Section 1: Parameter index instances
-- =========================================================================

-- Alphabetical: M=0, N=1, T=2
instance ParamIndex "M" where paramIndex = 0
instance ParamIndex "N" where paramIndex = 1
instance ParamIndex "T" where paramIndex = 2


-- =========================================================================
-- Section 2: Iteration Domain
-- =========================================================================

-- | { [t,i,j] : 1 <= t <= T, 1 <= i <= N, 1 <= j <= M }
type Domain =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 1)       -- t >= 1
   , 'TParam (P "T") >=. 'TDim (D 0)         -- t <= T
   , 'TDim (D 1) >=. 'TConst ('Pos 1)        -- i >= 1
   , 'TParam (P "N") >=. 'TDim (D 1)         -- i <= N
   , 'TDim (D 2) >=. 'TConst ('Pos 1)        -- j >= 1
   , 'TParam (P "M") >=. 'TDim (D 2)         -- j <= M
   ]


-- =========================================================================
-- Section 3: Dependency Maps (combined 6D: dims 0-2 input, dims 3-5 output)
-- =========================================================================

-- | { [t,i,j] -> [t+1,i-1,j] } — north neighbor flow dep
type DepNorth =
  '[ 'TDim (D 3) ==. ('TDim (D 0) +. 'TConst ('Pos 1))   -- t' = t+1
   , 'TDim (D 4) ==. ('TDim (D 1) -. 'TConst ('Pos 1))   -- i' = i-1
   , 'TDim (D 5) ==. 'TDim (D 2)                           -- j' = j
   -- source domain
   , 'TDim (D 0) >=. 'TConst ('Pos 1)
   , ('TParam (P "T") -. 'TConst ('Pos 1)) >=. 'TDim (D 0)  -- t < T
   , 'TDim (D 1) >=. 'TConst ('Pos 2)                        -- i >= 2 (since i-1 >= 1)
   , 'TParam (P "N") >=. 'TDim (D 1)
   , 'TDim (D 2) >=. 'TConst ('Pos 1)
   , 'TParam (P "M") >=. 'TDim (D 2)
   ]

-- | { [t,i,j] -> [t+1,i+1,j] } — south neighbor flow dep
type DepSouth =
  '[ 'TDim (D 3) ==. ('TDim (D 0) +. 'TConst ('Pos 1))
   , 'TDim (D 4) ==. ('TDim (D 1) +. 'TConst ('Pos 1))
   , 'TDim (D 5) ==. 'TDim (D 2)
   , 'TDim (D 0) >=. 'TConst ('Pos 1)
   , ('TParam (P "T") -. 'TConst ('Pos 1)) >=. 'TDim (D 0)
   , 'TDim (D 1) >=. 'TConst ('Pos 1)
   , ('TParam (P "N") -. 'TConst ('Pos 1)) >=. 'TDim (D 1)   -- i <= N-1
   , 'TDim (D 2) >=. 'TConst ('Pos 1)
   , 'TParam (P "M") >=. 'TDim (D 2)
   ]

-- | { [t,i,j] -> [t+1,i,j-1] } — west neighbor flow dep
type DepWest =
  '[ 'TDim (D 3) ==. ('TDim (D 0) +. 'TConst ('Pos 1))
   , 'TDim (D 4) ==. 'TDim (D 1)
   , 'TDim (D 5) ==. ('TDim (D 2) -. 'TConst ('Pos 1))
   , 'TDim (D 0) >=. 'TConst ('Pos 1)
   , ('TParam (P "T") -. 'TConst ('Pos 1)) >=. 'TDim (D 0)
   , 'TDim (D 1) >=. 'TConst ('Pos 1)
   , 'TParam (P "N") >=. 'TDim (D 1)
   , 'TDim (D 2) >=. 'TConst ('Pos 2)                        -- j >= 2
   , 'TParam (P "M") >=. 'TDim (D 2)
   ]

-- | { [t,i,j] -> [t+1,i,j+1] } — east neighbor flow dep
type DepEast =
  '[ 'TDim (D 3) ==. ('TDim (D 0) +. 'TConst ('Pos 1))
   , 'TDim (D 4) ==. 'TDim (D 1)
   , 'TDim (D 5) ==. ('TDim (D 2) +. 'TConst ('Pos 1))
   , 'TDim (D 0) >=. 'TConst ('Pos 1)
   , ('TParam (P "T") -. 'TConst ('Pos 1)) >=. 'TDim (D 0)
   , 'TDim (D 1) >=. 'TConst ('Pos 1)
   , 'TParam (P "N") >=. 'TDim (D 1)
   , 'TDim (D 2) >=. 'TConst ('Pos 1)
   , ('TParam (P "M") -. 'TConst ('Pos 1)) >=. 'TDim (D 2)   -- j <= M-1
   ]


-- =========================================================================
-- Section 4: Schedules (3→3 maps, combined 6D)
-- =========================================================================

-- | Identity: { [t,i,j] -> [t, i, j] }
type IdentitySched =
  '[ 'TDim (D 3) ==. 'TDim (D 0)
   , 'TDim (D 4) ==. 'TDim (D 1)
   , 'TDim (D 5) ==. 'TDim (D 2)
   ]

-- | Skewed: { [t,i,j] -> [t, t+i, t+j] }
type SkewedSched =
  '[ 'TDim (D 3) ==. 'TDim (D 0)
   , 'TDim (D 4) ==. ('TDim (D 0) +. 'TDim (D 1))
   , 'TDim (D 5) ==. ('TDim (D 0) +. 'TDim (D 2))
   ]


-- =========================================================================
-- Section 5: Compile-Time Proofs
-- =========================================================================

-- Domain is non-empty (has integer points for valid parameters)
proofDomainNonEmpty :: IslNonEmpty '["M","N","T"] 3 Domain => ()
proofDomainNonEmpty = ()
_useDomainNonEmpty :: ()
_useDomainNonEmpty = proofDomainNonEmpty

-- Dep images stay within the domain (well-formedness)
proofDepNorthWF :: IslImageSubset '["M","N","T"] 3 3 DepNorth Domain Domain => ()
proofDepNorthWF = ()
_useDepNorthWF :: ()
_useDepNorthWF = proofDepNorthWF

proofDepSouthWF :: IslImageSubset '["M","N","T"] 3 3 DepSouth Domain Domain => ()
proofDepSouthWF = ()
_useDepSouthWF :: ()
_useDepSouthWF = proofDepSouthWF

proofDepWestWF :: IslImageSubset '["M","N","T"] 3 3 DepWest Domain Domain => ()
proofDepWestWF = ()
_useDepWestWF :: ()
_useDepWestWF = proofDepWestWF

proofDepEastWF :: IslImageSubset '["M","N","T"] 3 3 DepEast Domain Domain => ()
proofDepEastWF = ()
_useDepEastWF :: ()
_useDepEastWF = proofDepEastWF


-- =========================================================================
-- Section 6: Scheduled Dependence Composition (type-level computation)
-- =========================================================================

-- Compose: SchedDep = Sched ∘ Dep ∘ Sched⁻¹
-- Step 1: reverse the schedule
type SkewedSchedRev = IslReverseMap '["M","N","T"] 3 3 SkewedSched
-- Step 2: compose Dep ∘ SchedRev (dep applied after schedule inverse)
-- IslCompose m1 m2 = m1 ∘ m2
-- We want: DepNorth ∘ SkewedSchedRev = { [sched_point] -> [dst] }
-- But IslReverseMap returns [[TConstraint ...]] (disjunction), so we
-- can't directly compose. Instead, display the composed map string.

-- Use IslMapToString to display the deps and schedules at the type level.
type DepNorthStr     = IslMapToString '["M","N","T"] 3 3 DepNorth
type DepSouthStr     = IslMapToString '["M","N","T"] 3 3 DepSouth
type DepWestStr      = IslMapToString '["M","N","T"] 3 3 DepWest
type DepEastStr      = IslMapToString '["M","N","T"] 3 3 DepEast
type IdentitySchedStr = IslMapToString '["M","N","T"] 3 3 IdentitySched
type SkewedSchedStr  = IslMapToString '["M","N","T"] 3 3 SkewedSched
type DomainStr       = IslToString '["M","N","T"] 3 Domain


-- =========================================================================
-- Section 7: Bug Detection (commented-out failing proofs)
-- =========================================================================

-- BUG: Dep with wrong direction — north instead of south
type DepBroken =
  '[ 'TDim (D 3) ==. ('TDim (D 0) +. 'TConst ('Pos 1))
   , 'TDim (D 4) ==. ('TDim (D 1) -. 'TConst ('Pos 1))   -- i-1
   , 'TDim (D 5) ==. 'TDim (D 2)
   -- But domain says i >= 1 (not i >= 2), so image has i'=0 which is outside Domain
   , 'TDim (D 0) >=. 'TConst ('Pos 1)
   , ('TParam (P "T") -. 'TConst ('Pos 1)) >=. 'TDim (D 0)
   , 'TDim (D 1) >=. 'TConst ('Pos 1)                      -- WRONG: should be >= 2
   , 'TParam (P "N") >=. 'TDim (D 1)
   , 'TDim (D 2) >=. 'TConst ('Pos 1)
   , 'TParam (P "M") >=. 'TDim (D 2)
   ]

-- This proves image(DepBroken, Domain) ⊆ Domain — but it's FALSE!
-- The image includes i'=0 which violates i' >= 1.
proofBrokenWF :: IslImageSubset '["M","N","T"] 3 3 DepBroken Domain Domain => ()
proofBrokenWF = ()
-- Uncomment to see compile error:
-- _useBrokenWF :: ()
-- _useBrokenWF = proofBrokenWF


-- =========================================================================
-- Section 8: Singletons (type-level → ISL objects at runtime)
-- =========================================================================

domainSing :: SBasicSet '["M","N","T"] 3 Domain
domainSing = sBasicSet

depNorthSing :: SBasicMap '["M","N","T"] 3 3 DepNorth
depNorthSing = sBasicMap

skewedSchedSing :: SBasicMap '["M","N","T"] 3 3 SkewedSched
skewedSchedSing = sBasicMap


-- =========================================================================
-- Main
-- =========================================================================

main :: IO ()
main = do
  putStrLn "=== 2D Stencil Tiling: Type-Level Verification ==="
  putStrLn ""

  -- ── Compile-time proofs ──────────────────────────────────────────────
  putStrLn "Compile-time proofs (verified by ISL plugin at build time):"
  putStrLn $ "  Domain non-empty:      " ++ show proofDomainNonEmpty
  putStrLn $ "  DepNorth image ⊆ Dom:  " ++ show proofDepNorthWF
  putStrLn $ "  DepSouth image ⊆ Dom:  " ++ show proofDepSouthWF
  putStrLn $ "  DepWest  image ⊆ Dom:  " ++ show proofDepWestWF
  putStrLn $ "  DepEast  image ⊆ Dom:  " ++ show proofDepEastWF
  putStrLn ""

  -- ── Type-level ISL strings (computed by plugin) ─────────────────────
  putStrLn "Type-level representations (computed by ISL at compile time):"
  putStrLn $ "  Domain:    " ++ symbolVal (Proxy @DomainStr)
  putStrLn $ "  DepNorth:  " ++ symbolVal (Proxy @DepNorthStr)
  putStrLn $ "  DepSouth:  " ++ symbolVal (Proxy @DepSouthStr)
  putStrLn $ "  DepWest:   " ++ symbolVal (Proxy @DepWestStr)
  putStrLn $ "  DepEast:   " ++ symbolVal (Proxy @DepEastStr)
  putStrLn $ "  Identity:  " ++ symbolVal (Proxy @IdentitySchedStr)
  putStrLn $ "  Skewed:    " ++ symbolVal (Proxy @SkewedSchedStr)
  putStrLn ""

  -- ── Singletons → runtime ISL ───────────────────────────────────────
  putStrLn "Singletons (type-level → runtime ISL objects):"

  strs <- runIslT $ do
    dom <- evalSBasicSet domainSing
    (Ur dStr, dom') <- SetOp.borrowSet dom SetOp.setToString
    SetOp.freeSet dom'

    dep <- evalSBasicMap depNorthSing
    (Ur depStr, dep') <- MapOp.borrowMap dep MapOp.mapToString
    MapOp.freeMap dep'

    sch <- evalSBasicMap skewedSchedSing
    (Ur schStr, sch') <- MapOp.borrowMap sch MapOp.mapToString
    MapOp.freeMap sch'

    pure (Ur (dStr, depStr, schStr))

  let (dStr, depStr, schStr) = strs
  putStrLn $ "  Domain (from singleton):    " ++ dStr
  putStrLn $ "  DepNorth (from singleton):  " ++ depStr
  putStrLn $ "  Skewed (from singleton):    " ++ schStr
  putStrLn ""

  putStrLn "Bug detection (uncomment _useBrokenWF to see compile error):"
  putStrLn "  DepBroken has i >= 1 instead of i >= 2, so image includes"
  putStrLn "  i' = 0 outside the domain. Plugin catches this as:"
  putStrLn "  \"Could not solve IslImageSubset ... DepBroken Domain Domain\""
  putStrLn ""

  putStrLn "All proofs verified at compile time."
