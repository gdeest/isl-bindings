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

-- | Cholesky factorisation as an Alpha system.
--
-- v7 surface form (named binders via @Alpha.Surface@).
--
-- Body equations (inside the declared domains):
--
-- @
-- L[i, j] = sqrt(A[i, i] - sum_k L[i, k]^2)                        for j = i (diagonal)
-- L[i, j] = (A[i, j] - sum_k L[i, k] * L[j, k]) / L[j, j]          for j < i (strict lower)
-- @
module Examples.Cholesky
  ( cholesky
  , CholeskyInputs
  , CholeskyOutputs
  , CholeskyLocals
  , CholeskyDecls
  , LowerTri
  , DiagN
  , StrictLowerN
  , DiagBodyN
  , StrictLowerBodyN
  ) where

import Data.Proxy (Proxy(..))

import Alpha.Surface
import Isl.TypeLevel.Constraint (TConstraint, type (>=.), type (<=.), type (==.))
import Isl.TypeLevel.Expr (D, P, TExpr(..), Z(..), type (-.))
import Isl.TypeLevel.Reflection (DomTag(..))

-- Importing Examples.Matmul brings in its 'ParamIndex "N"' orphan
-- instance transitively; we also reuse 'SquareN' as A's declared domain.
import Examples.Matmul (SquareN)


-- ═══════════════════════════════════════════════════════════════════════
-- Type-level domain synonyms (kept for test-harness imports)
-- ═══════════════════════════════════════════════════════════════════════

-- | The full lower triangle with diagonal:
-- @{(i, j) : 0 ≤ i ≤ N - 1, 0 ≤ j ≤ i}@.
type LowerTri =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. 'TDim (D 0)
   ] :: [TConstraint '["N"] 2]

-- | Diagonal branch: @{(i, j) : 0 ≤ i ≤ N - 1, j = i}@.
type DiagN =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 1) ==. 'TDim (D 0)
   ] :: [TConstraint '["N"] 2]

-- | Strict lower branch: @{(i, j) : 0 ≤ j ≤ i - 1, i ≤ N - 1}@.
type StrictLowerN =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. ('TDim (D 0) -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 2]

-- | 3D reduction body for the diagonal branch:
-- @{(i, j, k) : 0 ≤ i ≤ N - 1, j = i, 0 ≤ k ≤ j - 1}@.
type DiagBodyN =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 1) ==. 'TDim (D 0)
   , 'TDim (D 2) >=. 'TConst ('Pos 0)
   , 'TDim (D 2) <=. ('TDim (D 1) -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 3]

-- | 3D reduction body for the strict-lower branch:
-- @{(i, j, k) : 0 ≤ j ≤ i - 1, i ≤ N - 1, 0 ≤ k ≤ j - 1}@.
type StrictLowerBodyN =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. ('TDim (D 0) -. 'TConst ('Pos 1))
   , 'TDim (D 2) >=. 'TConst ('Pos 0)
   , 'TDim (D 2) <=. ('TDim (D 1) -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 3]


-- ═══════════════════════════════════════════════════════════════════════
-- Variable declarations
-- ═══════════════════════════════════════════════════════════════════════

type CholeskyInputs =
  '[ 'VarDecl @'["N"] @"A" @2 @('Literal SquareN) @Double
   ]

type CholeskyOutputs =
  '[ 'VarDecl @'["N"] @"L" @2 @('Literal LowerTri) @Double
   ]

type CholeskyLocals = '[]

type CholeskyDecls =
  '[ 'VarDecl @'["N"] @"A" @2 @('Literal SquareN)  @Double
   , 'VarDecl @'["N"] @"L" @2 @('Literal LowerTri) @Double
   ]


-- ═══════════════════════════════════════════════════════════════════════
-- Named-surface domain expressions
-- ═══════════════════════════════════════════════════════════════════════

-- | @{(i, j) | 0 ≤ i ≤ N - 1, 0 ≤ j ≤ i}@ in named form.
lowerTri :: DomExpr '["i", "j"] _
lowerTri = range0 @"N" #i /\ between (lit @0) #i #j

-- | Diagonal branch: @{(i, j) | 0 ≤ i ≤ N - 1, j = i}@.
diagN :: DomExpr '["i", "j"] _
diagN = range0 @"N" #i /\ #j .==. #i

-- | Strict lower branch: @{(i, j) | 0 ≤ j ≤ i - 1, i ≤ N - 1}@.
strictLowerN :: DomExpr '["i", "j"] _
strictLowerN = range0 @"N" #i /\ between (lit @0) (#i -. lit @1) #j

-- | 3D diagonal reduction body:
-- @{(i, j, k) | 0 ≤ i ≤ N - 1, j = i, 0 ≤ k ≤ j - 1}@.
diagBodyN :: DomExpr '["i", "j", "k"] _
diagBodyN = range0 @"N" #i /\ #j .==. #i /\ between (lit @0) (#j -. lit @1) #k

-- | 3D strict-lower reduction body:
-- @{(i, j, k) | 0 ≤ j ≤ i - 1, i ≤ N - 1, 0 ≤ k ≤ j - 1}@.
strictLowerBodyN :: DomExpr '["i", "j", "k"] _
strictLowerBodyN =
  range0 @"N" #i /\ between (lit @0) (#i -. lit @1) #j /\ between (lit @0) (#j -. lit @1) #k


-- ═══════════════════════════════════════════════════════════════════════
-- The Cholesky system (v7 surface form)
-- ═══════════════════════════════════════════════════════════════════════

cholesky :: System '["N"] _ _ _
cholesky = system
  ( Decls
      { dInputs  = input @"A" squareN (Proxy @Double)
                :> Nil
      , dOutputs = output @"L" lowerTri (Proxy @Double)
                :> Nil
      , dLocals  = Nil
      }
  )
  ( def @"L" @'["i", "j"]
      (caseB $
        when_ diagN
          -- Diagonal: sqrt(A[i, i] - sum_k L[i, k]^2)
          (mapB sqrt $
            at @"A" (ix2 #i #j) .-.
            sumOver @"k" diagBodyN
              (at @"L" (ix2 #i #k) .*. at @"L" (ix2 #i #k)))
      $ when_ strictLowerN
          -- Strict lower: (A[i, j] - sum_k L[i, k] * L[j, k]) / L[j, j]
          ((at @"A" (ix2 #i #j) .-.
            sumOver @"k" strictLowerBodyN
              (at @"L" (ix2 #i #k) .*. at @"L" (ix2 #j #k)))
           ./. at @"L" (ix2 #j #j))
      $ SBNil)
  :& EqNil )

-- Helper: re-use the squareN from Matmul but in scope ["i","j"].
squareN :: DomExpr '["i", "j"] _
squareN = range0 @"N" #i /\ range0 @"N" #j
