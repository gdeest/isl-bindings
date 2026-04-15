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

-- | A 2-output system: one matmul + one reader of the matmul output.
-- Used as a regression shape for the generic cross-read tile path.
-- All shape types are local to this file — the library stays
-- matmul-agnostic.
module Examples.MatmulNormalize
  ( matmulNormalize
  , MNInputs
  , MNOutputs
  , MNLocals
  , MNDecls
  , MNSquareN
  , MNCubeN
  ) where

import Data.Monoid (Sum(..))
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
import Examples.Matmul ()  -- for the ParamIndex "N" orphan instance
import Isl.TypeLevel.Constraint
  ( IslMultiAffToMap, TConstraint, type (>=.), type (<=.) )
import Isl.TypeLevel.Expr
  ( D, P, TExpr(..), Z(..), type (-.) )
import Isl.TypeLevel.Reflection (DomTag(..))


type MNSquareN =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 2]

type MNCubeN =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 2) >=. 'TConst ('Pos 0)
   , 'TDim (D 2) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 3]

type MNInputs =
  '[ 'VarDecl @'["N"] @"A" @2 @('Literal MNSquareN) @Double
   , 'VarDecl @'["N"] @"B" @2 @('Literal MNSquareN) @Double
   ]

type MNOutputs =
  '[ 'VarDecl @'["N"] @"C" @2 @('Literal MNSquareN) @Double
   , 'VarDecl @'["N"] @"D" @2 @('Literal MNSquareN) @Double
   ]

type MNLocals = '[]

type MNDecls =
  '[ 'VarDecl @'["N"] @"A" @2 @('Literal MNSquareN) @Double
   , 'VarDecl @'["N"] @"B" @2 @('Literal MNSquareN) @Double
   , 'VarDecl @'["N"] @"C" @2 @('Literal MNSquareN) @Double
   , 'VarDecl @'["N"] @"D" @2 @('Literal MNSquareN) @Double
   ]

type ProjectK =
  '[ 'TDim (D 0), 'TDim (D 1) ] :: [TExpr '["N"] 3]

type AccessA =
  '[ 'TDim (D 0), 'TDim (D 2) ] :: [TExpr '["N"] 3]

type AccessB =
  '[ 'TDim (D 2), 'TDim (D 1) ] :: [TExpr '["N"] 3]

type IdentityAccess =
  '[ 'TDim (D 0), 'TDim (D 1) ] :: [TExpr '["N"] 2]


matmulNormalize :: System '["N"] MNInputs MNOutputs MNLocals
matmulNormalize = System
  ( Decls
      { dInputs  = MkDecl :> MkDecl :> Nil
      , dOutputs = MkDecl :> MkDecl :> Nil
      , dLocals  = Nil
      }
  )
  (   Defines (Proxy @"C") cBody
  :&  Defines (Proxy @"D") dBody
  :&  EqNil
  )

cBody :: Expr '["N"] MNDecls 2 ('Literal MNSquareN) Double
cBody =
  PMap getSum $
    Reduce
      (Proxy :: Proxy (IslMultiAffToMap '["N"] 3 2 ProjectK))
      (PMap Sum reductionBody)

reductionBody :: Expr '["N"] MNDecls 3 ('Literal MNCubeN) Double
reductionBody =
  Pw (*)
    (Dep
      (Proxy :: Proxy (IslMultiAffToMap '["N"] 3 2 AccessA))
      (Var (Proxy @"A")))
    (Dep
      (Proxy :: Proxy (IslMultiAffToMap '["N"] 3 2 AccessB))
      (Var (Proxy @"B")))

dBody :: Expr '["N"] MNDecls 2 ('Literal MNSquareN) Double
dBody =
  Dep
    (Proxy :: Proxy (IslMultiAffToMap '["N"] 2 2 IdentityAccess))
    (Var (Proxy @"C"))
