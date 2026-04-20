{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Smoke tests for the ISL-backed checkers in "Alpha.Core.Tokens".
--
-- Goal of this turn: prove the wiring is live.  Construct minimal
-- 'NamedSet's / 'NamedMap's, call each checker, and assert the
-- Right/Left verdict matches ISL's answer.
module TokensSpec (tokensSpec) where

import Data.Proxy    (Proxy(..))
import Test.Tasty
import Test.Tasty.HUnit

import Isl.Typed.Constraints
  ( NamedSet(..)
  , NamedMap(..)
  , Conjunction(..)
  , Constraint(..)
  , Expr(..)
  , SetIx(..)
  , MapIx(..)
  )
import Alpha.Core.Tokens
  ( checkSubset
  , checkEqual
  , checkDisjoint
  , checkImageSubset
  , checkPartition
  , ElabError(..)
  )
import Alpha.Core.Reify (withFreshDom, withFreshMap, withFreshVar, reifyVar)
import Alpha.Scalar     (CNumType(..))

-- ═══════════════════════════════════════════════════════════════════════
-- Minimal NamedSet / NamedMap builders
-- ═══════════════════════════════════════════════════════════════════════

-- | @{ [i] : 0 <= i < N }@ — 1D domain of width N.
range1D :: Maybe String -> Integer -> NamedSet
range1D nm n = NamedSet
  { nsName   = nm
  , nsParams = []
  , nsNDims  = 1
  , nsConjs  =
      [ Conjunction
          [ InequalityConstraint (Ix (SetDim 0))                               -- i >= 0
          , InequalityConstraint (Add (Constant (n - 1)) (Mul (-1) (Ix (SetDim 0))))  -- i <= n-1
          ]
      ]
  }

-- | @{ [i] : lo <= i < hi }@.
interval1D :: Maybe String -> Integer -> Integer -> NamedSet
interval1D nm lo hi = NamedSet
  { nsName   = nm
  , nsParams = []
  , nsNDims  = 1
  , nsConjs  =
      [ Conjunction
          [ InequalityConstraint (Add (Ix (SetDim 0)) (Constant (negate lo)))
          , InequalityConstraint (Add (Constant (hi - 1)) (Mul (-1) (Ix (SetDim 0))))
          ]
      ]
  }

-- | Identity @{ [i] -> [i] : 0 <= i < n }@.
identity1D :: Integer -> NamedMap
identity1D n = NamedMap
  { nmDomainName = Nothing
  , nmRangeName  = Nothing
  , nmParams     = []
  , nmNIn        = 1
  , nmNOut       = 1
  , nmConjs      =
      [ Conjunction
          [ EqualityConstraint (Add (Ix (InDim 0)) (Mul (-1) (Ix (OutDim 0))))
          , InequalityConstraint (Ix (InDim 0))
          , InequalityConstraint (Add (Constant (n - 1)) (Mul (-1) (Ix (InDim 0))))
          ]
      ]
  }

-- ═══════════════════════════════════════════════════════════════════════
-- Tests
-- ═══════════════════════════════════════════════════════════════════════

tokensSpec :: TestTree
tokensSpec = testGroup "Alpha.Core.Tokens (ISL-backed checkers)"
  [ testCase "checkSubset: [0..9] ⊆ [0..9]" $ do
      res <- checkSubset @() @() (range1D Nothing 10) (range1D Nothing 10)
      case res of
        Right _ -> pure ()
        Left  e -> assertFailure ("expected Right, got Left " ++ show e)

  , testCase "checkSubset: [0..9] ⊄ [0..4]" $ do
      res <- checkSubset @() @() (range1D Nothing 10) (range1D Nothing 5)
      case res of
        Right _ -> assertFailure "expected Left, got Right"
        Left (SubsetFails _ _) -> pure ()
        Left e                 -> assertFailure ("unexpected error: " ++ show e)

  , testCase "checkEqual: [0..9] = [0..9]" $ do
      res <- checkEqual @() @() (range1D Nothing 10) (range1D Nothing 10)
      case res of
        Right _ -> pure ()
        Left  e -> assertFailure ("expected Right, got Left " ++ show e)

  , testCase "checkDisjoint: [0..4] ⊥ [5..9]" $ do
      res <- checkDisjoint @() @() (interval1D Nothing 0 5) (interval1D Nothing 5 10)
      case res of
        Right _ -> pure ()
        Left  e -> assertFailure ("expected Right, got Left " ++ show e)

  , testCase "checkDisjoint: [0..9] ∩ [5..19] ≠ ∅" $ do
      res <- checkDisjoint @() @() (interval1D Nothing 0 10) (interval1D Nothing 5 20)
      case res of
        Right _ -> assertFailure "expected Left, got Right"
        Left (DisjointFails _ _) -> pure ()
        Left e                   -> assertFailure ("unexpected error: " ++ show e)

  , testCase "checkImageSubset: identity on [0..9] ⊆ [0..9]" $ do
      res <- checkImageSubset @() @() @()
               (identity1D 10) (range1D Nothing 10) (range1D Nothing 10)
      case res of
        Right _ -> pure ()
        Left  e -> assertFailure ("expected Right, got Left " ++ show e)

  , testCase "checkImageSubset: identity on [0..9] ⊄ [0..4]" $ do
      res <- checkImageSubset @() @() @()
               (identity1D 10) (range1D Nothing 10) (range1D Nothing 5)
      case res of
        Right _ -> assertFailure "expected Left, got Right"
        Left (ImageSubsetFails _ _ _) -> pure ()
        Left e                        -> assertFailure ("unexpected error: " ++ show e)

  , testCase "checkPartition: [0..4] and [5..9] partition [0..9]" $ do
      res <- checkPartition @() @'[]
               (interval1D Nothing 0 10)
               [interval1D Nothing 0 5, interval1D Nothing 5 10]
      case res of
        Right _ -> pure ()
        Left  e -> assertFailure ("expected Right, got Left " ++ show e)

  , testCase "checkPartition: overlapping branches rejected" $ do
      res <- checkPartition @() @'[]
               (interval1D Nothing 0 10)
               [interval1D Nothing 0 6, interval1D Nothing 5 10]
      case res of
        Right _ -> assertFailure "expected Left, got Right"
        Left (NonPartitionDisjoint 0 1) -> pure ()
        Left e -> assertFailure ("unexpected error: " ++ show e)

  , testCase "checkPartition: cover gap rejected" $ do
      res <- checkPartition @() @'[]
               (interval1D Nothing 0 10)
               [interval1D Nothing 0 3, interval1D Nothing 5 10]
      case res of
        Right _ -> assertFailure "expected Left, got Right"
        Left (NonPartitionCover _) -> pure ()
        Left e -> assertFailure ("unexpected error: " ++ show e)

  , testGroup "Alpha.Core.Reify (CPS installers)"
      [ testCase "withFreshDom installs domain skolem" $ do
          let dom = range1D Nothing 10
              observed = withFreshDom (Proxy @()) dom $ \_ -> (42 :: Int)
          observed @?= 42

      , testCase "withFreshMap installs map skolem" $ do
          let m = identity1D 10
              observed = withFreshMap (Proxy @()) m $ \_ -> (42 :: Int)
          observed @?= 42

      , testCase "withFreshVar installs var skolem and reifies triple" $ do
          let dom = range1D Nothing 10
              triple = ("x", 2, CFloat64)
              observed =
                withFreshVar (Proxy @()) ("x", 2, CFloat64, dom) $ \pv _ ->
                  reifyVar (Proxy @()) pv
          observed @?= triple
      ]
  ]
