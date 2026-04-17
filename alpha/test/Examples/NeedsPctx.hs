{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Minimal system whose body obligation is /non-trivially/ pctx-dependent.
--
-- The output is @y[0] = x[N-1]@ on a single-point output domain
-- @{[i]: i = 0}@ reading from input domain @{[k]: 0 ≤ k ≤ N-1}@.  The
-- access image is the singleton @{[N-1]}@; that set is a subset of
-- the input domain iff @N ≥ 1@ (at @N = 0@ the input is empty and
-- @N-1 = -1@ is outside it).
--
-- Under the plan's pctx-fusion refactor, building 'needsPctx' requires
-- the plugin to discharge @IslImageSubset@ under the pctx @[N ≥ 1]@.
-- Weakening the pctx to @'[]@ must make the plugin reject the
-- obligation with counterexample @{[-1+N]: N ≤ 0}@.
module Examples.NeedsPctx
  ( needsPctx
  , NeedsPctxPctx
  , NeedsPctxInputs
  , NeedsPctxOutputs
  , NeedsPctxLocals
  ) where

import Data.Proxy (Proxy(..))

import Alpha.Surface
import Isl.TypeLevel.Constraint (TConstraint, type (>=.), type (==.), type (<=.))
import Isl.TypeLevel.Expr (D, P, TExpr(..), Z(..), type (-.))
import Isl.TypeLevel.Reflection (DomTag(..))


-- | Parameter precondition: @N ≥ 1@.
type NeedsPctxPctx =
  '[ 'TParam (P "N") >=. 'TConst ('Pos 1) ]
    :: [TConstraint '["N"] 0]

-- | Input domain: @{[k] : 0 ≤ k ≤ N − 1}@.
type NeedsPctxIn =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 1]

-- | Output domain: @{[i] : i = 0}@ — a single point, non-empty for
-- every valid @N@.  The body reads @x[N-1]@, whose image on the
-- output domain is @{[N-1]}@.
type NeedsPctxOut =
  '[ 'TDim (D 0) ==. 'TConst ('Pos 0)
   ] :: [TConstraint '["N"] 1]

type NeedsPctxInputs =
  '[ 'VarDecl @'["N"] @"x" @1 @('Literal NeedsPctxIn) @Double ]

type NeedsPctxOutputs =
  '[ 'VarDecl @'["N"] @"y" @1 @('Literal NeedsPctxOut) @Double ]

type NeedsPctxLocals =
  '[]


-- Named-surface domain expressions for the Surface constructors.

inDom :: DomExpr '["k"] _
inDom = range0 @"N" #k

outDom :: DomExpr '["i"] _
outDom = #i .==. lit @0


-- | The load-bearing system: construction succeeds with pctx @[N ≥ 1]@.
--
-- Weakening the pctx to @'[]@ causes the plugin to reject the
-- @at \@"x" (par \@"N" -. lit \@1)@ access: the image
-- @{[i + N - 1] : i = 0} = {[N - 1]}@ is not a subset of
-- @{[k] : 0 ≤ k ≤ N - 1}@ without the @N ≥ 1@ precondition.
needsPctx :: System '["N"] NeedsPctxPctx _ _ _
needsPctx = system
  ( Decls
      { dInputs  = input  @"x" inDom  (Proxy @Double) :> Nil
      , dOutputs = output @"y" outDom (Proxy @Double) :> Nil
      , dLocals  = Nil
      }
  )
  ( def @"y" @'["i"]
      (at @"x" (ix1 (par @"N" -. lit @1)))
   :& EqNil )
