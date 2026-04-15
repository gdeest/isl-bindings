{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}

-- | Compile-time test for union-aware type families (U-suffix).
-- The plugin rewrites these type families at compile time;
-- compilation success = all assertions hold.
module Examples.UnionCompose
  ( projectedStr
  , proofCompiles
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (symbolVal)
import Isl.TypeLevel
import Isl.TypeLevel.Sing (KnownDisjunction, knownDisjunction, STDisjunction)

-- A triangle: { [i,j] : 0 <= j <= i <= N }
type Triangle = '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
                 , 'TParam (P "N") >=. 'TDim (D 0)
                 , 'TDim (D 1) >=. 'TConst ('Pos 0)
                 , 'TDim (D 0) >=. 'TDim (D 1) ]

-- A rectangle: { [i,j] : 0 <= i <= N and 0 <= j <= N }
type Rectangle = '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
                  , 'TParam (P "N") >=. 'TDim (D 0)
                  , 'TDim (D 1) >=. 'TConst ('Pos 0)
                  , 'TParam (P "N") >=. 'TDim (D 1) ]

-- Complement of triangle (union of polyhedra — NOT a single basic set!)
type CompTriangle = IslComplementSetU '["N"] 2 '[Triangle]

-- Difference: Rectangle \ Triangle (upper triangle)
type UpperTriangle = IslDifferenceSetU '["N"] 2 '[Rectangle] '[Triangle]

-- Intersection: CompTriangle ∩ Rectangle (should equal UpperTriangle)
type CompInRect = IslIntersectSetU '["N"] 2 CompTriangle '[Rectangle]

-- Chained composition: complement then project out
type Projected = IslProjectOutU '["N"] 2 1 1 1 CompTriangle

-- ToString on a union result
projectedStr :: String
projectedStr = symbolVal (Proxy @(IslToStringU '["N"] 1 Projected))

-- Proof obligations discharged at compile time within this module:
-- CompInRect ⊆ Rectangle
_proofSubset :: IslSubsetU '["N"] 2 CompInRect '[Rectangle] => ()
_proofSubset = ()

-- CompInRect = UpperTriangle
_proofEqual :: IslEqualU '["N"] 2 CompInRect UpperTriangle => ()
_proofEqual = ()

-- UpperTriangle is non-empty
_proofNonEmpty :: IslNonEmptyU '["N"] 2 UpperTriangle => ()
_proofNonEmpty = ()

-- Test: can GHC derive KnownDisjunction for a plugin-computed union type?
_testKD :: STDisjunction '["N"] 2 UpperTriangle
_testKD = knownDisjunction

-- This function forces GHC to solve all proof constraints above.
proofCompiles :: Bool
proofCompiles = _proofSubset `seq` _proofEqual `seq` _proofNonEmpty `seq` True
