{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}

-- | Singleton-based type-level polyhedral DSL demo.
--
-- All proof obligations are verified at compile time by the ISL plugin.
-- Singleton-carrying polyhedra are evaluated to ISL objects at runtime.
module Main where

import Isl.TypeLevel
import Isl.HighLevel.Context (runIslT, Ur(..))
import qualified Isl.HighLevel.Set as SetOp
import qualified Isl.HighLevel.Map as MapOp

-- =========================================================================
-- Parameter index instances (required for singleton reification)
-- =========================================================================

instance ParamIndex "N" where paramIndex = 0

-- =========================================================================
-- Type-level set definitions
-- =========================================================================

-- | { (i, j) : 0 <= i <= N, 0 <= j <= i }
type Triangle =
  '[ 'TDim (D 0)     >=. 'TConst ('Pos 0)
   , 'TParam (P "N") >=. 'TDim (D 0)
   , 'TDim (D 1)     >=. 'TConst ('Pos 0)
   , 'TDim (D 0)     >=. 'TDim (D 1)
   ]

-- | { (i, j) : 0 <= i <= N, 0 <= j <= N }
type Rectangle =
  '[ 'TDim (D 0)     >=. 'TConst ('Pos 0)
   , 'TParam (P "N") >=. 'TDim (D 0)
   , 'TDim (D 1)     >=. 'TConst ('Pos 0)
   , 'TParam (P "N") >=. 'TDim (D 1)
   ]

-- =========================================================================
-- Type-level map definitions
-- =========================================================================

-- | Shift map: { (i, j) -> (i+1, j) }
-- In combined (ni+no=4) space: dim 0,1 = in, dim 2,3 = out
type ShiftRight =
  '[ 'TDim (D 2) ==. ('TDim (D 0) +. 'TConst ('Pos 1))
   , 'TDim (D 3) ==. 'TDim (D 1)
   ]

-- | Identity map: { (i, j) -> (i, j) }
type Identity2D =
  '[ 'TDim (D 2) ==. 'TDim (D 0)
   , 'TDim (D 3) ==. 'TDim (D 1)
   ]

-- | Expected image: { (i, j) : 1 <= i <= N+1, 0 <= j <= N }
type ShiftedRectangle =
  '[ 'TDim (D 0)     >=. 'TConst ('Pos 1)
   , ('TParam (P "N") +. 'TConst ('Pos 1)) >=. 'TDim (D 0)
   , 'TDim (D 1)     >=. 'TConst ('Pos 0)
   , 'TParam (P "N") >=. 'TDim (D 1)
   ]

-- =========================================================================
-- Compile-time proofs (plugin-verified assertions)
-- =========================================================================

-- | Triangle c Rectangle
proofSubset :: IslSubset '["N"] 2 Triangle Rectangle => ()
proofSubset = ()

-- | Identity = Identity (map equality)
proofMapEqual :: IslMapEqual '["N"] 2 2 Identity2D Identity2D => ()
proofMapEqual = ()

-- | image(Rectangle, ShiftRight) c ShiftedRectangle
proofImageSubset :: IslImageSubset '["N"] 2 2 ShiftRight Rectangle ShiftedRectangle => ()
proofImageSubset = ()

-- =========================================================================
-- Compile-time COMPUTATIONS (plugin-rewritten type families)
-- =========================================================================

-- | The complement of Triangle within the universe — computed by ISL at
-- compile time. This is a concrete type-level value, not an assertion.
type TriangleComplement = IslComplementSet '["N"] 2 Triangle

-- | The image of Rectangle under ShiftRight — computed by ISL.
type ComputedShift = IslApply '["N"] 2 2 ShiftRight Rectangle

-- | The range of the shift map — computed by ISL.
type ShiftRange = IslRangeTF '["N"] 2 2 ShiftRight

-- | Project out dimension 1 (j) from the Triangle, leaving only the i range.
-- This is existential quantification: { i : ∃j. (i,j) ∈ Triangle }
-- Result: { i : 0 <= i <= N }
type TriangleProjectJ = IslProjectOut '["N"] 2 1 1 1 Triangle

-- | Parse an ISL set directly from string notation.
-- This covers modular constraints, existentials, floor/ceiling — anything ISL supports.
type EvenPoints = IslFromString '["N"] 1
  "[N] -> { [i] : exists k: i = 2k and 0 <= i <= N }"

-- | A 2D tiling constraint parsed from ISL string: points where both
-- coordinates are divisible by a tile size.
type TiledPoints = IslFromString '["N"] 2
  "[N] -> { [i, j] : exists (ki, kj : i = 4ki and j = 4kj) and 0 <= i,j <= N }"

-- =========================================================================
-- Singleton-carrying polyhedra (auto-derived from type-level info)
-- =========================================================================

triangle :: SBasicSet '["N"] 2 Triangle
triangle = sBasicSet

rectangle :: SBasicSet '["N"] 2 Rectangle
rectangle = sBasicSet

shiftRight :: SBasicMap '["N"] 2 2 ShiftRight
shiftRight = sBasicMap

shiftedRect :: SBasicSet '["N"] 2 ShiftedRectangle
shiftedRect = sBasicSet

-- =========================================================================
-- Runtime evaluation
-- =========================================================================

main :: IO ()
main = do
  putStrLn "=== Singleton-Based Type-Level Polyhedral DSL ==="

  -- Compile-time proofs (the fact that this module compiled means they hold)
  putStrLn ""
  putStrLn "Compile-time proofs:"
  putStrLn $ "  Triangle c Rectangle:          " ++ show proofSubset
  putStrLn $ "  Identity = Identity:            " ++ show proofMapEqual
  putStrLn $ "  shift(Rectangle) c ShiftedRect: " ++ show proofImageSubset

  -- Evaluate singleton-carrying sets to ISL objects
  putStrLn ""
  putStrLn "Runtime evaluation via singletons:"

  strs <- runIslT $ do
    triSet <- evalSBasicSet triangle
    (Ur triStr, triSet') <- SetOp.borrowSet triSet SetOp.setToString
    SetOp.freeSet triSet'

    rectSet <- evalSBasicSet rectangle
    (Ur rectStr, rectSet') <- SetOp.borrowSet rectSet SetOp.setToString
    SetOp.freeSet rectSet'

    -- Evaluate map and apply it
    shiftMap <- evalSBasicMap shiftRight
    srcSet   <- evalSBasicSet rectangle
    shifted  <- MapOp.apply shiftMap srcSet
    (Ur shiftedStr, shifted') <- SetOp.borrowSet shifted SetOp.setToString
    SetOp.freeSet shifted'

    dstSet <- evalSBasicSet shiftedRect
    (Ur dstStr, dstSet') <- SetOp.borrowSet dstSet SetOp.setToString
    SetOp.freeSet dstSet'

    pure (Ur (triStr, rectStr, shiftedStr, dstStr))

  let (triStr, rectStr, shiftedStr, dstStr) = strs
  putStrLn $ "  Triangle      = " ++ triStr
  putStrLn $ "  Rectangle     = " ++ rectStr
  putStrLn $ "  shift(Rect)   = " ++ shiftedStr
  putStrLn $ "  ShiftedRect   = " ++ dstStr

  putStrLn ""
  putStrLn "All proofs verified at compile time, all evaluations succeeded."
