{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Floyd-Warshall all-pairs shortest-path as an Alpha system.
--
-- v7 surface form (named binders via @Alpha.Surface@).
--
-- @
-- D[k,  i, j] = min(D[k-1, i, j], D[k-1, i, k] + D[k-1, k, j])   (k >= 1)
-- D[0,  i, j] = A[i, j]                                            (k = 0)
-- Result[i, j] = D[N-1, i, j]
-- @
module Examples.FloydWarshall
  ( floyd
  , FWInputs
  , FWOutputs
  , FWLocals
  , FWDecls
  , FWCube3D
  , FWKEq0_3D
  , FWKGe1_3D
  , NGeOne
  , NeedsNGeOneSrc
  , NeedsNGeOneDst
  , hasParamCtxDemoPositive
  ) where

import Data.Proxy (Proxy(..))

import Alpha.Surface
import Isl.TypeLevel.Constraint
  ( HasParamCtx
  , IslSubset(..)
  , TConstraint
  , type (>=.)
  , type (<=.)
  , type (==.)
  )
import Isl.TypeLevel.Expr (D, P, TExpr(..), Z(..), type (-.))
import Isl.TypeLevel.Reflection (DomTag(..))

-- Importing Examples.Matmul (for SquareN) brings in its
-- 'ParamIndex "N"' orphan instance transitively.
import Examples.Matmul (SquareN)


-- ═══════════════════════════════════════════════════════════════════════
-- Parameter precondition
-- ═══════════════════════════════════════════════════════════════════════

type NGeOne =
  '[ 'TParam (P "N") >=. 'TConst ('Pos 1)
   ] :: [TConstraint '["N"] 0]


-- ═══════════════════════════════════════════════════════════════════════
-- Type-level domain synonyms (kept for test-harness imports)
-- ═══════════════════════════════════════════════════════════════════════

-- | The 3D cube: @{(k, i, j) : 0 ≤ k, i, j ≤ N - 1}@.
type FWCube3D =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 2) >=. 'TConst ('Pos 0)
   , 'TDim (D 2) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 3]

-- | k = 0 branch: @{(0, i, j) : 0 ≤ i, j ≤ N - 1}@.
type FWKEq0_3D =
  '[ 'TDim (D 0) ==. 'TConst ('Pos 0)
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 2) >=. 'TConst ('Pos 0)
   , 'TDim (D 2) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 3]

-- | k >= 1 branch: @{(k, i, j) : 1 ≤ k ≤ N - 1, 0 ≤ i, j ≤ N - 1}@.
type FWKGe1_3D =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 1)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 2) >=. 'TConst ('Pos 0)
   , 'TDim (D 2) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 3]


-- ═══════════════════════════════════════════════════════════════════════
-- Variable declarations
-- ═══════════════════════════════════════════════════════════════════════

type FWInputs =
  '[ 'VarDecl @'["N"] @"A" @2 @('Literal SquareN) @Double
   ]

type FWOutputs =
  '[ 'VarDecl @'["N"] @"Result" @2 @('Literal SquareN) @Double
   ]

type FWLocals =
  '[ 'VarDecl @'["N"] @"D" @3 @('Literal FWCube3D) @Double
   ]

type FWDecls =
  '[ 'VarDecl @'["N"] @"A"      @2 @('Literal SquareN)  @Double
   , 'VarDecl @'["N"] @"Result" @2 @('Literal SquareN)  @Double
   , 'VarDecl @'["N"] @"D"      @3 @('Literal FWCube3D) @Double
   ]


-- ═══════════════════════════════════════════════════════════════════════
-- Named-surface domain expressions
-- ═══════════════════════════════════════════════════════════════════════

-- | @{(i, j) | 0 ≤ i, j ≤ N - 1}@ in named form (for declarations).
squareN :: DomExpr '["i", "j"] _
squareN = range0 @"N" #i /\ range0 @"N" #j

-- | 3D cube @{(k, i, j) | 0 ≤ k, i, j ≤ N - 1}@ in named form.
fwCube3D :: DomExpr '["k", "i", "j"] _
fwCube3D = range0 @"N" #k /\ range0 @"N" #i /\ range0 @"N" #j

-- | k = 0 branch domain.
fwKEq0 :: DomExpr '["k", "i", "j"] _
fwKEq0 = #k .==. lit @0 /\ range0 @"N" #i /\ range0 @"N" #j

-- | k >= 1 branch domain.
fwKGe1 :: DomExpr '["k", "i", "j"] _
fwKGe1 = between (lit @1) (par @"N" -. lit @1) #k /\ range0 @"N" #i /\ range0 @"N" #j


-- ═══════════════════════════════════════════════════════════════════════
-- The Floyd-Warshall system (v7 surface form)
-- ═══════════════════════════════════════════════════════════════════════

floyd :: System '["N"] _ _ _
floyd = system
  ( Decls
      { dInputs  = input @"A" squareN (Proxy @Double)
                :> Nil
      , dOutputs = output @"Result" squareN (Proxy @Double)
                :> Nil
      , dLocals  = local @"D" fwCube3D (Proxy @Double)
                :> Nil
      }
  )
  ( def @"D" @'["k", "i", "j"]
      (caseB $
        when_ fwKEq0
          -- k = 0: D[0, i, j] = A[i, j]
          (at @"A" (ix2 #i #j))
      $ when_ fwKGe1
          -- k >= 1: D[k, i, j] = min(D[k-1, i, j], D[k-1, i, k] + D[k-1, k, j])
          (pwB min
            (at @"D" (ix3 (#k -. lit @1) #i #j))
            (at @"D" (ix3 (#k -. lit @1) #i #k)
                  .+. at @"D" (ix3 (#k -. lit @1) #k #j)))
      $ SBNil)
 :& def @"Result" @'["i", "j"]
      -- Result[i, j] = D[N - 1, i, j]
      (at @"D" (ix3 (par @"N" -. lit @1) #i #j))
 :& EqNil )


-- ═══════════════════════════════════════════════════════════════════════
-- Positive demonstration of the 'HasParamCtx' plugin path (v3 / D19)
-- ═══════════════════════════════════════════════════════════════════════

-- 1-dim source: @{ [k] : k == N - 1 }@.  Non-empty for all @N@.
type NeedsNGeOneSrc =
  '[ 'TDim (D 0) ==. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 1]

-- 1-dim target: @{ [k] : 0 ≤ k ≤ N - 1 }@.  Empty when @N ≤ 0@.
type NeedsNGeOneDst =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 1]

-- | Positive demo: requires the 'HasParamCtx' given at this
-- binding's compile time.
hasParamCtxDemoPositive
  :: HasParamCtx '["N"] NGeOne => ()
hasParamCtxDemoPositive =
  islSubsetEv @'["N"] @1 @NeedsNGeOneSrc @NeedsNGeOneDst
