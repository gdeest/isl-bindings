{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Negative reflected-route demo: replaceInputDomain rejecting an
-- unsafe restriction.
--
-- Builds matmul, computes a *narrowed* A domain via 'islIntersect',
-- and asks 'replaceInputDomain' to install it.  The transform's
-- runtime obligation check ('islSubsetCheck') notices that the
-- narrowed domain does NOT contain the access image SquareN, and
-- returns 'Left'.  The test asserts that the result is indeed 'Left'
-- and that the error variant is 'DomainShrinkUnsafe'.
--
-- This exercises the runtime obligation-failure path of the
-- transform layer.  Combined with the positive demo, the two tests
-- prove that 'replaceInputDomain' actually consults ISL at runtime —
-- it's not a no-op gate.
module Examples.ReflectedMatmulFails (runReflectedMatmulNegative) where

import Data.Proxy (Proxy(..))
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure)

import Alpha.Transform.Domain (replaceInputDomain, TransformError(..))
import Examples.Matmul (matmul, SquareN)
import Isl.TypeLevel.Constraint (TConstraint, type (>=.), type (<=.))
import Isl.TypeLevel.Expr
  ( D
  , P
  , TExpr(..)
  , Z(..)
  , type (-.)
  , type (+.)
  )
import Isl.TypeLevel.Reflection
  ( DomTag(..)
  , KnownDom
  , islIntersect
  , reflectDomString
  )
import Isl.TypeLevel.Sing (ParamIndex(..))


-- "Top half rows": { (i, j) | 0 ≤ i ≤ N/2, 0 ≤ j ≤ N - 1 }.
-- Strictly smaller than SquareN — excludes rows with i > N/2.
-- Intersecting SquareN with this gives the top-half region, which
-- does NOT contain SquareN's bottom rows.
type TopHalfRows =
  '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
   , ('TConst ('Pos 2) +. 'TDim (D 0)) <=. 'TParam (P "N")
   , 'TDim (D 1) >=. 'TConst ('Pos 0)
   , 'TDim (D 1) <=. ('TParam (P "N") -. 'TConst ('Pos 1))
   ] :: [TConstraint '["N"] 2]


runReflectedMatmulNegative :: Assertion
runReflectedMatmulNegative =
  -- Compute the narrowed A domain at runtime via islIntersect.
  -- The result is the intersection of SquareN with TopHalfRows,
  -- which excludes SquareN's bottom rows.
  islIntersect @'["N"] @2 @('Literal SquareN) @('Literal TopHalfRows) Proxy Proxy $ \narrowTag -> do
    result <- replaceInputDomain @"A" @'["N"] @2
                Proxy
                (Proxy :: Proxy ('Literal SquareN))   -- expected access image
                narrowTag                              -- proposed new domain
                matmul
                (pure ())
    case result of
      Left (DomainShrinkUnsafe nameStr imgStr newStr) -> do
        assertBool "error variable name is A" (nameStr == "A")
        assertBool "error image string is non-empty" (not (null imgStr))
        assertBool "error new-domain string is non-empty" (not (null newStr))
      Right () ->
        assertFailure
          "expected replaceInputDomain to reject the narrowed A domain, \
          \but the runtime obligation check passed"
