{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | The smallest possible system: @y[i] = 0@ on a rank-1 line.
--
-- This is the v6.2 minimum-viable shape for the polymorphic
-- 'Alpha.Transform.Tile.tile' transform.  No inputs, no Dep, no
-- Reduce — just a single output equation with a 'Const' body.
--
-- Used to demonstrate that 'tile' correctly:
--
--   * Validates the runtime tile factor list against the target's
--     declared rank.
--   * Builds the new tiled domain ISL string from the original
--     domain string and the runtime factors.
--   * Reifies the new domain into a fresh 'ReflectedTag'.
--   * Short-circuits on empty tiled domains via 'islNonEmpty'.
--   * Returns a 'TileResult' whose new target carries the new rank
--     and new reflected tag.
module Examples.Zero1D
  ( zero1D
  , Zero1DInputs
  , Zero1DOutputs
  , Zero1DLocals
  , LineN
  ) where

import Data.Proxy (Proxy(..))

import Alpha.Core
  ( Decl(..)
  , DeclList(..)
  , Decls(..)
  , Equation(..)
  , EqList(..)
  , Expr(..)
  , System
  , pattern System
  , VarDecl(..)
  )
import Examples.Matmul ()  -- ParamIndex "N" orphan
import Isl.TypeLevel.Constraint
  ( TConstraint, type (>=.), type (<=.) )
import Isl.TypeLevel.Expr
  ( D, P, TExpr(..), Z(..), type (-.) )
import Isl.TypeLevel.Reflection (DomTag(..))


-- | The rank-1 line: @{ i | 0 <= i <= N - 1 }@.
type LineN =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 1]

type Zero1DInputs = '[]
type Zero1DOutputs =
  '[ 'VarDecl @'["N"] @"y" @1 @('Literal LineN) @Double
   ]
type Zero1DLocals = '[]

zero1D :: System '["N"] Zero1DInputs Zero1DOutputs Zero1DLocals
zero1D = System
  ( Decls
      { dInputs  = Nil
      , dOutputs = MkDecl :> Nil
      , dLocals  = Nil
      }
  )
  ( Defines (Proxy @"y") (Const 0.0) :& EqNil )
