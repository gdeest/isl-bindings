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

-- | LU decomposition (Doolittle form) as an Alpha system.
--
-- v7 surface form (named binders via @Alpha.Surface@).
--
-- Body equations:
--
-- @
-- U[i, j] = A[i, j] - sum_{k=0..i-1} L[i, k] * U[k, j]                for (i, j) in UUpper
-- L[i, j] = (A[i, j] - sum_{k=0..j-1} L[i, k] * U[k, j]) / U[j, j]    for (i, j) in LStrict
-- @
module Examples.LU
  ( luDecomp
  , LUInputs
  , LUOutputs
  , LULocals
  , LUDecls
  , LStrict
  , UUpper
  , LBody3D
  , UBody3D
  ) where

import Data.Proxy (Proxy(..))

import Alpha.Surface
import Isl.TypeLevel.Constraint (TConstraint, type (>=.), type (<=.))
import Isl.TypeLevel.Expr (D, P, TExpr(..), Z(..), type (-.))
import Isl.TypeLevel.Reflection (DomTag(..))

-- Importing Examples.Matmul brings its 'ParamIndex "N"' orphan
-- instance into scope transitively; we also reuse 'SquareN'.
import Examples.Matmul (SquareN)


-- ═══════════════════════════════════════════════════════════════════════
-- Type-level domain synonyms (kept for test-harness imports)
-- ═══════════════════════════════════════════════════════════════════════

-- | L's ambient domain: the strict lower triangle
-- @{(i, j) : 0 ≤ j ≤ i - 1, 0 ≤ i ≤ N - 1}@.
type LStrict =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. ('TDim (D 0) -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 2]

-- | U's ambient domain: the upper triangle including the diagonal
-- @{(i, j) : 0 ≤ i ≤ j ≤ N - 1}@.
type UUpper =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. 'TDim (D 1)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 2]

-- | 3D reduction body for L's equation:
-- @{(i, j, k) : 0 ≤ j ≤ i - 1 ≤ N - 2, 0 ≤ k ≤ j - 1}@.
type LBody3D =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. ('TDim (D 0) -. 'TConst ('Pos 1))
   , 'TDim (D 2) >=. 'TConst ('Pos 0)
   , 'TDim (D 2) <=. ('TDim (D 1) -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 3]

-- | 3D reduction body for U's equation:
-- @{(i, j, k) : 0 ≤ i ≤ j ≤ N - 1, 0 ≤ k ≤ i - 1}@.
type UBody3D =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. 'TDim (D 1)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 2) >=. 'TConst ('Pos 0)
   , 'TDim (D 2) <=. ('TDim (D 0) -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 3]


-- ═══════════════════════════════════════════════════════════════════════
-- Variable declarations
-- ═══════════════════════════════════════════════════════════════════════

type LUInputs =
  '[ 'VarDecl @'["N"] @"A" @2 @('Literal SquareN) @Double
   ]

type LUOutputs =
  '[ 'VarDecl @'["N"] @"L" @2 @('Literal LStrict) @Double
   , 'VarDecl @'["N"] @"U" @2 @('Literal UUpper)  @Double
   ]

type LULocals = '[]

type LUDecls =
  '[ 'VarDecl @'["N"] @"A" @2 @('Literal SquareN) @Double
   , 'VarDecl @'["N"] @"L" @2 @('Literal LStrict) @Double
   , 'VarDecl @'["N"] @"U" @2 @('Literal UUpper)  @Double
   ]


-- ═══════════════════════════════════════════════════════════════════════
-- Named-surface domain expressions
-- ═══════════════════════════════════════════════════════════════════════

-- | @{(i, j) | 0 ≤ i ≤ N - 1, 0 ≤ j ≤ N - 1}@ in named form.
squareN :: DomExpr '["i", "j"] _
squareN = range0 @"N" #i /\ range0 @"N" #j

-- | L's domain: strict lower triangle.
lStrict :: DomExpr '["i", "j"] _
lStrict = range0 @"N" #i /\ between (lit @0) (#i -. lit @1) #j

-- | U's domain: upper triangle with diagonal.
uUpper :: DomExpr '["i", "j"] _
uUpper = between (lit @0) #j #i /\ #j .<=. par @"N" -. lit @1

-- | 3D reduction body for L: @0 ≤ j ≤ i - 1, 0 ≤ k ≤ j - 1@.
lBody3D :: DomExpr '["i", "j", "k"] _
lBody3D =
  range0 @"N" #i /\ between (lit @0) (#i -. lit @1) #j /\ between (lit @0) (#j -. lit @1) #k

-- | 3D reduction body for U: @0 ≤ i ≤ j ≤ N - 1, 0 ≤ k ≤ i - 1@.
uBody3D :: DomExpr '["i", "j", "k"] _
uBody3D =
  between (lit @0) #j #i /\ #j .<=. par @"N" -. lit @1 /\ between (lit @0) (#i -. lit @1) #k


-- ═══════════════════════════════════════════════════════════════════════
-- The LU decomposition system (v7 surface form)
-- ═══════════════════════════════════════════════════════════════════════

luDecomp :: System '["N"] _ _ _
luDecomp = system
  ( Decls
      { dInputs  = input @"A" squareN (Proxy @Double)
                :> Nil
      , dOutputs = output @"L" lStrict (Proxy @Double)
                :> output @"U" uUpper  (Proxy @Double)
                :> Nil
      , dLocals  = Nil
      }
  )
  ( def @"L" @'["i", "j"]
      -- L[i, j] = (A[i, j] - sum_k L[i, k] * U[k, j]) / U[j, j]
      ((at @"A" (ix2 #i #j) .-.
        sumOver @"k" lBody3D
          (at @"L" (ix2 #i #k) .*. at @"U" (ix2 #k #j)))
       ./. at @"U" (ix2 #j #j))
 :& def @"U" @'["i", "j"]
      -- U[i, j] = A[i, j] - sum_k L[i, k] * U[k, j]
      (at @"A" (ix2 #i #j) .-.
        sumOver @"k" uBody3D
          (at @"L" (ix2 #i #k) .*. at @"U" (ix2 #k #j)))
 :& EqNil )
