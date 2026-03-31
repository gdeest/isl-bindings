{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}

-- | Type-level multi-aff demo: all proofs are verified at compile time.
-- If this module compiles, all assertions hold.
module Main where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (symbolVal)

import Isl.TypeLevel
import Isl.HighLevel.Context (runIslT, Ur(..))
import qualified Isl.HighLevel.MultiAff as MAOp
import qualified Isl.HighLevel.Map as MapOp
import qualified Isl.HighLevel.Set as SetOp

-- =========================================================================
-- Parameter index instances
-- =========================================================================

instance ParamIndex "N" where paramIndex = 0

-- =========================================================================
-- Multi-aff definitions (expression lists)
-- =========================================================================

-- | Shift map: { [i, j] -> [i+1, j] }
-- Functional form: output 0 = i+1, output 1 = j
type ShiftRight = '[ 'TDim (D 0) +. 'TConst ('Pos 1), 'TDim (D 1) ]

-- | Identity: { [i, j] -> [i, j] }
type Identity2D = '[ 'TDim (D 0), 'TDim (D 1) ]

-- | Scale by 2: { [i, j] -> [2*i, 2*j] }
type ScaleByTwo = '[ 'TMul ('Pos 2) ('TDim (D 0)), 'TMul ('Pos 2) ('TDim (D 1)) ]

-- | Rectangle set: { [i, j] : 0 <= i <= N, 0 <= j <= N }
type Rectangle =
  '[ 'TDim (D 0)     >=. 'TConst ('Pos 0)
   , 'TParam (P "N") >=. 'TDim (D 0)
   , 'TDim (D 1)     >=. 'TConst ('Pos 0)
   , 'TParam (P "N") >=. 'TDim (D 1)
   ]

-- | The same shift map in relational (constraint) form for comparison:
type ShiftRightMap =
  '[ 'TDim (D 2) ==. ('TDim (D 0) +. 'TConst ('Pos 1))
   , 'TDim (D 3) ==. 'TDim (D 1)
   ]

-- =========================================================================
-- Compile-time proofs
-- =========================================================================

-- | MultiAffToMap converts functional form to relational form,
-- and the result is map-equal to the hand-written relational form.
proofMultiAffToMapCorrect
  :: IslMapEqual '["N"] 2 2
       (IslMultiAffToMap '["N"] 2 2 ShiftRight)
       ShiftRightMap
  => ()
proofMultiAffToMapCorrect = ()

-- | Multi-aff equality: identity equals identity.
proofIdentityEqual
  :: IslMultiAffEqual '["N"] 2 2 Identity2D Identity2D => ()
proofIdentityEqual = ()

-- =========================================================================
-- Compile-time computations
-- =========================================================================

-- | Render a multi-aff to ISL string.
type ShiftString = IslMultiAffToString '["N"] 2 2 ShiftRight

-- | Apply multi-aff to a set.
type ShiftedRect = IslApplyMultiAff '["N"] 2 2 ShiftRight Rectangle

-- | Compose: ScaleByTwo after ShiftRight should give [2*(i+1), 2*j]
type Composed = IslComposeMultiAff '["N"] 2 2 2 ScaleByTwo ShiftRight

-- =========================================================================
-- Singleton-carrying multi-affs
-- =========================================================================

shift :: SMultiAff '["N"] 2 2 ShiftRight
shift = sMultiAff

identity2d :: SMultiAff '["N"] 2 2 Identity2D
identity2d = sMultiAff

scaleBy2 :: SMultiAff '["N"] 2 2 ScaleByTwo
scaleBy2 = sMultiAff

-- =========================================================================
-- Runtime evaluation
-- =========================================================================

main :: IO ()
main = do
  putStrLn "=== Type-Level Multi-Aff Demo ==="

  -- Compile-time proofs (the fact that this module compiled proves them)
  putStrLn ""
  putStrLn "Compile-time proofs (verified by compilation):"
  putStrLn $ "  MultiAffToMap(ShiftRight) = ShiftRightMap:  " ++ show proofMultiAffToMapCorrect
  putStrLn $ "  Identity2D = Identity2D:                    " ++ show proofIdentityEqual

  -- Compile-time computations
  putStrLn ""
  putStrLn "Compile-time computations:"
  putStrLn $ "  ShiftRight as ISL string: " ++ symbolVal (Proxy @ShiftString)

  -- Singleton evaluation
  putStrLn ""
  putStrLn "Singleton evaluation:"

  strs <- runIslT $ do
    shiftMA <- evalSMultiAff shift
    (Ur shiftStr, shiftMA') <- MAOp.borrowMA shiftMA MAOp.maToString
    MAOp.freeMA shiftMA'

    identMA <- evalSMultiAff identity2d
    (Ur identStr, identMA') <- MAOp.borrowMA identMA MAOp.maToString
    MAOp.freeMA identMA'

    scaleMA <- evalSMultiAff scaleBy2
    (Ur scaleStr, scaleMA') <- MAOp.borrowMA scaleMA MAOp.maToString
    MAOp.freeMA scaleMA'

    -- Convert ShiftRight to map and verify
    shiftMA2 <- evalSMultiAff shift
    shiftMap <- MAOp.toMap shiftMA2
    (Ur mapStr, shiftMap') <- MapOp.borrowMap shiftMap MapOp.mapToString
    MapOp.freeMap shiftMap'

    pure (Ur (shiftStr, identStr, scaleStr, mapStr))

  let (shiftStr, identStr, scaleStr, mapStr) = strs
  putStrLn $ "  ShiftRight  = " ++ shiftStr
  putStrLn $ "  Identity2D  = " ++ identStr
  putStrLn $ "  ScaleByTwo  = " ++ scaleStr
  putStrLn $ "  ShiftRight as Map = " ++ mapStr

  putStrLn ""
  putStrLn "All proofs verified at compile time, all evaluations succeeded."
