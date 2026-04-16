{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-partial-type-signatures #-}

-- | Minimal Int32 'ReduceMax' kernel for regression test #2:
-- @y[i] = max_k A[i, k]@ on @[0, N-1]^2@.
--
-- The integer reduction exercises the @sdReduceIdentity@ path — the
-- emitted init loop must use @INT32_MIN@ as the identity, not a float
-- literal (the pre-fix bug #2).
module Examples.IntRowMax
  ( intRowMax
  , IntRowMaxInputs
  , IntRowMaxOutputs
  , IntRowMaxLocals
  , IntRowMaxDecls
  ) where

import Data.Int (Int32)
import Data.Proxy (Proxy(..))

import Alpha.Surface
import Isl.TypeLevel.Constraint (TConstraint, type (>=.), type (<=.))
import Isl.TypeLevel.Expr (D, P, TExpr(..), Z(..), type (-.))
import Isl.TypeLevel.Reflection (DomTag(..))


type SquareN =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 2]

type LineN =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 1]

type IntRowMaxInputs =
  '[ 'VarDecl @'["N"] @"A" @2 @('Literal SquareN) @Int32 ]

type IntRowMaxOutputs =
  '[ 'VarDecl @'["N"] @"y" @1 @('Literal LineN) @Int32 ]

type IntRowMaxLocals = '[]

type IntRowMaxDecls =
  '[ 'VarDecl @'["N"] @"A" @2 @('Literal SquareN) @Int32
   , 'VarDecl @'["N"] @"y" @1 @('Literal LineN) @Int32
   ]

lineN :: DomExpr '["i"] _
lineN = range0 @"N" #i

squareN :: DomExpr '["i", "k"] _
squareN = range0 @"N" #i /\ range0 @"N" #k

intRowMax :: System '["N"] IntRowMaxInputs IntRowMaxOutputs IntRowMaxLocals
intRowMax = system
  ( Decls
      { dInputs  = input @"A" squareN (Proxy @Int32) :> Nil
      , dOutputs = output @"y" lineN  (Proxy @Int32) :> Nil
      , dLocals  = Nil
      }
  )
  ( def @"y" @'["i"]
      (reduceOver @"k" ReduceMax squareN
        (at @"A" (ix2 #i #k)))
   :& EqNil )
