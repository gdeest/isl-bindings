{-# LANGUAGE BangPatterns #-}
module Test.TypedConstraints (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Isl.Typed.Constraints

tests :: TestTree
tests = testGroup "Isl.Typed.Constraints"
  [ testCase "expandExpr linear" $ do
      -- 3*x0 + 2*x1 + 5
      let e = Add (Mul 3 (Ix (SetDim 0))) (Add (Mul 2 (Ix (SetDim 1))) (Constant 5))
          (coeffs, c) = expandExpr e
      assertEqual "constant" 5 c
      assertEqual "coeffs" [(3, SetDim 0), (2, SetDim 1)] coeffs

  , testCase "rebuildExpr round-trip" $ do
      let coeffs = [(3, SetDim 0), (2, SetDim 1)]
          c = 5
          e = rebuildExpr coeffs c
          (coeffs', c') = expandExpr e
      assertEqual "constant preserved" c c'
      assertEqual "coeffs preserved" coeffs coeffs'

  , testCase "hasFloorDiv detects floor division" $ do
      assertBool "plain" (not $ hasFloorDiv (Ix (SetDim 0) :: Expr SetIx))
      assertBool "constant" (not $ hasFloorDiv (Constant 5 :: Expr SetIx))
      assertBool "floor" (hasFloorDiv (FloorDiv (Ix (SetDim 0)) 3 :: Expr SetIx))
      assertBool "nested" (hasFloorDiv (Add (Ix (SetDim 0)) (FloorDiv (Ix (SetDim 1)) 2) :: Expr SetIx))

  , testCase "modExpr decomposes correctly" $ do
      -- modExpr a b = a - b * floor(a/b)
      let e = modExpr (Ix (SetDim 0)) 3
      assertBool "modExpr should contain FloorDiv" (hasFloorDiv e)

  , testCase "conjunctionHasFloorDiv" $ do
      let c1 = InequalityConstraint (Ix (SetDim 0) :: Expr SetIx)
          c2 = EqualityConstraint (FloorDiv (Ix (SetDim 0)) 2 :: Expr SetIx)
      assertBool "no div" (not $ conjunctionHasFloorDiv [c1])
      assertBool "has div" (conjunctionHasFloorDiv [c1, c2])

  , testCase "DSL operators produce correct constraints" $ do
      let x = idx (SetDim 0)
          n = idx (SetParam 0)
          c = x >=: cst 0 &&: x <=: n
          Conjunction cs = c
      assertEqual "should have 2 constraints" 2 (length cs)
  ]
