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
{-# OPTIONS_GHC -Wno-orphans #-}

-- | 3-D heat equation (six-neighbour averaging) as an Alpha system.
--
-- v7 surface form (named binders via @Alpha.Surface@).
--
-- Stencil formula:
--
-- @
-- u[0, i, j, k] = u0[i, j, k]                                           (t = 0)
-- u[t, i, j, k] = 0                                                     (t >= 1, boundary)
-- u[t, i, j, k] = (u[t-1, i-1, j, k] + u[t-1, i+1, j, k]
--                + u[t-1, i, j-1, k] + u[t-1, i, j+1, k]
--                + u[t-1, i, j, k-1] + u[t-1, i, j, k+1]) / 6           (t >= 1, interior)
-- @
module Examples.Heat3D
  ( heat3D
  , H3Inputs
  , H3Outputs
  , H3Locals
  , H3Decls
  , SpaceBox
  , TimeBox
  , T0
  , Face_i0
  , Face_iN
  , Face_j0
  , Face_jN
  , Face_k0
  , Face_kN
  , Interior
  ) where

import Data.Proxy (Proxy(..))

import Alpha.Surface
import Isl.TypeLevel.Constraint (TConstraint, type (>=.), type (<=.), type (==.))
import Isl.TypeLevel.Expr (D, P, TExpr(..), Z(..), type (-.))

import Isl.TypeLevel.Reflection (DomTag(..))
import Isl.TypeLevel.Sing (ParamIndex(..))

-- Import 'Examples.Matmul' for its 'ParamIndex "N"' orphan instance.
import Examples.Matmul ()


-- ═══════════════════════════════════════════════════════════════════════
-- Parameter indices
-- ═══════════════════════════════════════════════════════════════════════

-- "N" is index 0 (imported transitively from Examples.Matmul).
-- "T" is new; ISL convention is alphabetical, so "N" < "T" => "T" = 1.
instance ParamIndex "T" where paramIndex = 1


-- ═══════════════════════════════════════════════════════════════════════
-- Type-level domain synonyms (kept for test-harness imports)
-- ═══════════════════════════════════════════════════════════════════════

-- | The 3-D spatial box: @{(i, j, k) : 0 <= i, j, k <= N - 1}@.
type SpaceBox =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 2) >=. 'TConst ('Pos 0)
   , 'TDim (D 2) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N","T"] 3]

-- | The 4-D space-time box: @{(t, i, j, k) : 0 <= t <= T - 1, 0 <= i, j, k <= N - 1}@.
type TimeBox =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "T") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 2) >=. 'TConst ('Pos 0)
   , 'TDim (D 2) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 3) >=. 'TConst ('Pos 0)
   , 'TDim (D 3) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N","T"] 4]


-- ═══════════════════════════════════════════════════════════════════════
-- Branch domains (refinements of TimeBox)
-- ═══════════════════════════════════════════════════════════════════════

type T0 =
  '[ 'TDim (D 0) ==. 'TConst ('Pos 0)
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 2) >=. 'TConst ('Pos 0)
   , 'TDim (D 2) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 3) >=. 'TConst ('Pos 0)
   , 'TDim (D 3) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N","T"] 4]

type Face_i0 =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 1)
   , 'TDim (D 0) <=. ('TParam (P "T") -. 'TConst ('Pos 1))
   , 'TDim (D 1) ==. 'TConst ('Pos 0)
   , 'TDim (D 2) >=. 'TConst ('Pos 0)
   , 'TDim (D 2) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 3) >=. 'TConst ('Pos 0)
   , 'TDim (D 3) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N","T"] 4]

type Face_iN =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 1)
   , 'TDim (D 0) <=. ('TParam (P "T") -. 'TConst ('Pos 1))
   , 'TDim (D 1) ==. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 1)
   , 'TDim (D 2) >=. 'TConst ('Pos 0)
   , 'TDim (D 2) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 3) >=. 'TConst ('Pos 0)
   , 'TDim (D 3) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N","T"] 4]

type Face_j0 =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 1)
   , 'TDim (D 0) <=. ('TParam (P "T") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 1)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 2))
   , 'TDim (D 2) ==. 'TConst ('Pos 0)
   , 'TDim (D 3) >=. 'TConst ('Pos 0)
   , 'TDim (D 3) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N","T"] 4]

type Face_jN =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 1)
   , 'TDim (D 0) <=. ('TParam (P "T") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 1)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 2))
   , 'TDim (D 2) ==. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 2) >=. 'TConst ('Pos 1)
   , 'TDim (D 3) >=. 'TConst ('Pos 0)
   , 'TDim (D 3) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N","T"] 4]

type Face_k0 =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 1)
   , 'TDim (D 0) <=. ('TParam (P "T") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 1)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 2))
   , 'TDim (D 2) >=. 'TConst ('Pos 1)
   , 'TDim (D 2) <=. ('TParam (P "N") -. 'TConst ('Pos 2))
   , 'TDim (D 3) ==. 'TConst ('Pos 0)
   ] :: [TConstraint '["N","T"] 4]

type Face_kN =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 1)
   , 'TDim (D 0) <=. ('TParam (P "T") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 1)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 2))
   , 'TDim (D 2) >=. 'TConst ('Pos 1)
   , 'TDim (D 2) <=. ('TParam (P "N") -. 'TConst ('Pos 2))
   , 'TDim (D 3) ==. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 3) >=. 'TConst ('Pos 1)
   ] :: [TConstraint '["N","T"] 4]

type Interior =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 1)
   , 'TDim (D 0) <=. ('TParam (P "T") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 1)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 2))
   , 'TDim (D 2) >=. 'TConst ('Pos 1)
   , 'TDim (D 2) <=. ('TParam (P "N") -. 'TConst ('Pos 2))
   , 'TDim (D 3) >=. 'TConst ('Pos 1)
   , 'TDim (D 3) <=. ('TParam (P "N") -. 'TConst ('Pos 2))
   ] :: [TConstraint '["N","T"] 4]


-- ═══════════════════════════════════════════════════════════════════════
-- Variable declarations
-- ═══════════════════════════════════════════════════════════════════════

type H3Inputs =
  '[ 'VarDecl @'["N","T"] @"u0" @3 @('Literal SpaceBox) @Double
   ]

type H3Outputs =
  '[ 'VarDecl @'["N","T"] @"u"  @4 @('Literal TimeBox)  @Double
   ]

type H3Locals = '[]

type H3Decls =
  '[ 'VarDecl @'["N","T"] @"u0" @3 @('Literal SpaceBox) @Double
   , 'VarDecl @'["N","T"] @"u"  @4 @('Literal TimeBox)  @Double
   ]


-- ═══════════════════════════════════════════════════════════════════════
-- Named-surface domain expressions
-- ═══════════════════════════════════════════════════════════════════════

-- | Spatial box for u0: @{(i, j, k) | 0 <= i, j, k <= N - 1}@.
spaceBox :: DomExpr '["i", "j", "k"] _
spaceBox = range0 @"N" #i /\ range0 @"N" #j /\ range0 @"N" #k

-- | Space-time box for u: @{(t, i, j, k) | 0 <= t <= T - 1, 0 <= i, j, k <= N - 1}@.
timeBox :: DomExpr '["t", "i", "j", "k"] _
timeBox = range0 @"T" #t /\ range0 @"N" #i /\ range0 @"N" #j /\ range0 @"N" #k

-- | t = 0 branch.
t0Dom :: DomExpr '["t", "i", "j", "k"] _
t0Dom = #t .==. lit @0 /\ range0 @"N" #i /\ range0 @"N" #j /\ range0 @"N" #k

-- | Left-i face: @{t >= 1, i = 0, 0 <= j, k <= N - 1}@.
faceI0Dom :: DomExpr '["t", "i", "j", "k"] _
faceI0Dom =
  between (lit @1) (par @"T" -. lit @1) #t /\ #i .==. lit @0 /\ range0 @"N" #j /\ range0 @"N" #k

-- | Right-i face: @{t >= 1, i = N - 1, i >= 1, 0 <= j, k <= N - 1}@.
faceINDom :: DomExpr '["t", "i", "j", "k"] _
faceINDom =
  between (lit @1) (par @"T" -. lit @1) #t
  /\ #i .==. par @"N" -. lit @1 /\ #i .>=. lit @1
  /\ range0 @"N" #j /\ range0 @"N" #k

-- | Front-j face: @{t >= 1, 1 <= i <= N - 2, j = 0, 0 <= k <= N - 1}@.
faceJ0Dom :: DomExpr '["t", "i", "j", "k"] _
faceJ0Dom =
  between (lit @1) (par @"T" -. lit @1) #t
  /\ between (lit @1) (par @"N" -. lit @2) #i
  /\ #j .==. lit @0 /\ range0 @"N" #k

-- | Back-j face: @{t >= 1, 1 <= i <= N - 2, j = N - 1, j >= 1, 0 <= k <= N - 1}@.
faceJNDom :: DomExpr '["t", "i", "j", "k"] _
faceJNDom =
  between (lit @1) (par @"T" -. lit @1) #t
  /\ between (lit @1) (par @"N" -. lit @2) #i
  /\ #j .==. par @"N" -. lit @1 /\ #j .>=. lit @1
  /\ range0 @"N" #k

-- | Bottom-k face: @{t >= 1, 1 <= i, j <= N - 2, k = 0}@.
faceK0Dom :: DomExpr '["t", "i", "j", "k"] _
faceK0Dom =
  between (lit @1) (par @"T" -. lit @1) #t
  /\ between (lit @1) (par @"N" -. lit @2) #i
  /\ between (lit @1) (par @"N" -. lit @2) #j
  /\ #k .==. lit @0

-- | Top-k face: @{t >= 1, 1 <= i, j <= N - 2, k = N - 1, k >= 1}@.
faceKNDom :: DomExpr '["t", "i", "j", "k"] _
faceKNDom =
  between (lit @1) (par @"T" -. lit @1) #t
  /\ between (lit @1) (par @"N" -. lit @2) #i
  /\ between (lit @1) (par @"N" -. lit @2) #j
  /\ #k .==. par @"N" -. lit @1 /\ #k .>=. lit @1

-- | Interior: @{1 <= t <= T - 1, 1 <= i, j, k <= N - 2}@.
interiorDom :: DomExpr '["t", "i", "j", "k"] _
interiorDom =
  between (lit @1) (par @"T" -. lit @1) #t
  /\ between (lit @1) (par @"N" -. lit @2) #i
  /\ between (lit @1) (par @"N" -. lit @2) #j
  /\ between (lit @1) (par @"N" -. lit @2) #k


-- ═══════════════════════════════════════════════════════════════════════
-- The Heat3D system (v7 surface form)
-- ═══════════════════════════════════════════════════════════════════════

heat3D :: System '["N","T"] _ _ _
heat3D = system
  ( Decls
      { dInputs  = input @"u0" spaceBox (Proxy @Double)
                :> Nil
      , dOutputs = output @"u" timeBox (Proxy @Double)
                :> Nil
      , dLocals  = Nil
      }
  )
  ( def @"u" @'["t", "i", "j", "k"]
      (caseB $
        when_ t0Dom
          -- t = 0: u[0, i, j, k] = u0[i, j, k]
          (at @"u0" (ix3 #i #j #k))
      $ when_ faceI0Dom  (litB 0)
      $ when_ faceINDom  (litB 0)
      $ when_ faceJ0Dom  (litB 0)
      $ when_ faceJNDom  (litB 0)
      $ when_ faceK0Dom  (litB 0)
      $ when_ faceKNDom  (litB 0)
      $ when_ interiorDom
          -- Interior: six-neighbour average
          (mapB (/ 6) $
                at @"u" (ix4 (#t -. lit @1) (#i -. lit @1) #j #k)
            .+. at @"u" (ix4 (#t -. lit @1) (#i +. lit @1) #j #k)
            .+. at @"u" (ix4 (#t -. lit @1) #i (#j -. lit @1) #k)
            .+. at @"u" (ix4 (#t -. lit @1) #i (#j +. lit @1) #k)
            .+. at @"u" (ix4 (#t -. lit @1) #i #j (#k -. lit @1))
            .+. at @"u" (ix4 (#t -. lit @1) #i #j (#k +. lit @1)))
      $ SBNil)
  :& EqNil )
