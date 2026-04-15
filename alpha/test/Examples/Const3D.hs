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

-- | Rank-3 Const-only system: @w[i, j, k] = 1.0@ on the N-cube.
--
-- Used as a v6.2 test fixture for the polymorphic 'tile' transform
-- at rank 3.  Drives both full-rank and partial-rank tile tests:
--
--   * Full: @tileAll @"w" [2, 2, 2]@ — every dim tiled.
--   * Partial: @tile @"w" [Just 2, Nothing, Just 2]@ — middle dim
--     untouched, the load-bearing proof that 'Nothing' / "leave
--     this dim alone" works end-to-end.
module Examples.Const3D
  ( const3D
  , Const3DInputs
  , Const3DOutputs
  , Const3DLocals
  , CubeN3
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


-- | The 3D N-cube: @{ (i, j, k) | 0 <= i, j, k <= N - 1 }@.
type CubeN3 =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 2) >=. 'TConst ('Pos 0)
   , 'TDim (D 2) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 3]

type Const3DInputs = '[]
type Const3DOutputs =
  '[ 'VarDecl @'["N"] @"w" @3 @('Literal CubeN3) @Double
   ]
type Const3DLocals = '[]

const3D :: System '["N"] Const3DInputs Const3DOutputs Const3DLocals
const3D = System
  ( Decls
      { dInputs  = Nil
      , dOutputs = MkDecl :> Nil
      , dLocals  = Nil
      }
  )
  ( Defines (Proxy @"w") (Const 1.0) :& EqNil )
