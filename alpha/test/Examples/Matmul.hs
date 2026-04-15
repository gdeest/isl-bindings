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

-- | Matrix multiplication as an Alpha system (v7 surface form).
--
-- @
-- C[i, j] = ∑_k A[i, k] * B[k, j]    for 0 ≤ i, j < N
-- @
module Examples.Matmul
  ( matmul
  , MatmulInputs
  , MatmulOutputs
  , MatmulLocals
  , MatmulDecls
  , SquareN
  , CubeN
  ) where

import Data.Proxy (Proxy(..))

import Alpha.Surface
import Isl.TypeLevel.Constraint (TConstraint, type (>=.), type (<=.))
import Isl.TypeLevel.Expr (D, P, TExpr(..), Z(..), type (-.))
import Isl.TypeLevel.Reflection (DomTag(..))
import Isl.TypeLevel.Sing (ParamIndex(..))


instance ParamIndex "N" where paramIndex = 0


-- ═══════════════════════════════════════════════════════════════════════
-- Type-level domain synonyms (kept for downstream consumers)
-- ═══════════════════════════════════════════════════════════════════════

type SquareN =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 2]

type CubeN =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , 'TDim (D 0) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   , 'TDim (D 2) >=. 'TConst ('Pos 0)
   , 'TDim (D 2) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 3]

type MatmulInputs =
  '[ 'VarDecl @'["N"] @"A" @2 @('Literal SquareN) @Double
   , 'VarDecl @'["N"] @"B" @2 @('Literal SquareN) @Double
   ]

type MatmulOutputs =
  '[ 'VarDecl @'["N"] @"C" @2 @('Literal SquareN) @Double
   ]

type MatmulLocals = '[]

type MatmulDecls = '[ 'VarDecl @'["N"] @"A" @2 @('Literal SquareN) @Double
                    , 'VarDecl @'["N"] @"B" @2 @('Literal SquareN) @Double
                    , 'VarDecl @'["N"] @"C" @2 @('Literal SquareN) @Double
                    ]


-- ═══════════════════════════════════════════════════════════════════════
-- The matmul system
-- ═══════════════════════════════════════════════════════════════════════

squareN :: DomExpr '["i", "j"] _
squareN = range0 @"N" #i /\ range0 @"N" #j

cubeN :: DomExpr '["i", "j", "k"] _
cubeN = range0 @"N" #i /\ range0 @"N" #j /\ range0 @"N" #k

matmul :: System '["N"] _ _ _
matmul = system
  ( Decls
      { dInputs  = input @"A" squareN (Proxy @Double)
                :> input @"B" squareN (Proxy @Double)
                :> Nil
      , dOutputs = output @"C" squareN (Proxy @Double)
                :> Nil
      , dLocals  = Nil
      }
  )
  ( def @"C" @'["i", "j"]
      (sumOver @"k" cubeN
        (at @"A" (ix2 #i #k) .*. at @"B" (ix2 #k #j)))
   :& EqNil )
