{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-partial-type-signatures #-}

-- | Test system for Dep rewriting during reindex.
--
-- @
-- A[i, j] = 0.0               — const body (target for reindex)
-- B[i, j] = A[i, j]           — reads A via Dep (non-target, has Dep→Var@A)
-- @
--
-- Reindexing A with skew [i, s] → [i, s - i] should:
--   1. Rewrite A's body (Const, trivial)
--   2. Rewrite B's Dep map to A: old {[i,j] → [i,j]} becomes
--      composed with reverse-skew to target A's new coords
module Examples.DepReindex
  ( depSystem
  , runReindexDep
  ) where

import Data.Proxy (Proxy(..))
import Test.Tasty.HUnit (Assertion, assertFailure)

import Alpha.Surface
import Alpha.Transform.Reindex (reindex)
import Isl.TypeLevel.Constraint (TConstraint, type (>=.), type (<=.))
import Isl.TypeLevel.Expr (D, P, TExpr(..), Z(..), type (-.))
import Isl.TypeLevel.Reflection (DomTag(..))
import Isl.TypeLevel.Sing (ParamIndex(..))

-- Reuse ParamIndex from Matmul
import Examples.Matmul ()

-- | The domain: square {0 <= i,j <= N-1}
squareN :: DomExpr '["i", "j"] _
squareN = range0 @"N" #i /\ range0 @"N" #j

-- | A[i,j] = 0.0, B[i,j] = A[i,j]
depSystem :: System '["N"] _ _ _
depSystem = system
  ( Decls
      { dInputs  = Nil
      , dOutputs = output @"A" squareN (Proxy @Double)
                :> output @"B" squareN (Proxy @Double)
                :> Nil
      , dLocals  = Nil
      }
  )
  ( def @"A" @'["i", "j"] (litB 0.0)
   :& def @"B" @'["i", "j"] (at @"A" (ix2 #i #j))
   :& EqNil )


-- | Skew map: [i, s] → [i, s - i]  (new coords → old coords)
-- This means s = i + j, so j = s - i.
type SkewMap = '[ 'TDim (D 0)
               , 'TAdd ('TDim (D 1)) ('TMul ('Neg 1) ('TDim (D 0)))
               ] :: [TExpr '["N"] 2]


-- | Reindex "A" with the skew map.  B's Dep to A should be rewritten.
runReindexDep :: Assertion
runReindexDep =
  case reindex "A" 2 (type SkewMap) depSystem of
    Left err ->
      assertFailure ("reindex dep system failed: " ++ show err)
    Right _newSys ->
      -- Success: the walker handled B's Dep(Var "A") by composing
      -- the access map with the reverse of the skew map and verifying
      -- the image-subset obligation at runtime via ISL.
      pure ()
