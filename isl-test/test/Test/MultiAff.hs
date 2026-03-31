{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.MultiAff (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.Monad (Ur(..))

import qualified Isl.HighLevel.MultiAff as MA
import qualified Isl.HighLevel.Map as Map
import qualified Isl.HighLevel.BasicMap as BM
import qualified Isl.HighLevel.Set as Set

import Test.Helpers

tests :: TestTree
tests = testGroup "MultiAff"
  [ unitTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit"
  [ testCase "mkMultiAff identity produces valid string" $ do
      let result = runIslTest $ do
            ma <- MA.mkMultiAff @'[] @2 @2 $ \Nil (x :- y :- Nil) ->
              [idx x, idx y]
            (Ur str, ma') <- MA.borrowMA ma MA.maToString
            MA.freeMA ma'
            return (Ur str)
      assertBool "should contain arrow" ("->" `isInfixOfS` result)

  , testCase "mkMultiAff shift produces valid string" $ do
      let result = runIslTest $ do
            ma <- MA.mkMultiAff @'[] @2 @2 $ \Nil (x :- y :- Nil) ->
              [idx x +: cst 1, idx y]
            (Ur str, ma') <- MA.borrowMA ma MA.maToString
            MA.freeMA ma'
            return (Ur str)
      assertBool "should contain arrow" ("->" `isInfixOfS` result)

  , testCase "toBasicMap produces correct map" $ do
      let eq = runIslTest $ do
            -- Build multi-aff: [i,j] -> [i+1, j]
            ma <- MA.mkMultiAff @'[] @2 @2 $ \Nil (x :- y :- Nil) ->
              [idx x +: cst 1, idx y]
            -- Convert to basic map
            bm <- MA.toBasicMap ma
            -- Build the same map directly via constraints
            bm2 <- BM.mkBasicMap @'[] @2 @2 $ \Nil (i :- j :- Nil) (o0 :- o1 :- Nil) ->
              idx o0 ==: idx i +: cst 1 &&: idx o1 ==: idx j
            -- Check equality
            (Ur result, _, _) <- BM.isEqual bm bm2
            BM.freeBM bm
            BM.freeBM bm2
            return (Ur result)
      assertBool "multi-aff → basic map should equal hand-built map" eq

  , testCase "pullback composes correctly" $ do
      let eq = runIslTest $ do
            -- f: [i,j] -> [2*i, 2*j]
            f <- MA.mkMultiAff @'[] @2 @2 $ \Nil (x :- y :- Nil) ->
              [2 *: idx x, 2 *: idx y]
            -- g: [i,j] -> [i+1, j]
            g <- MA.mkMultiAff @'[] @2 @2 $ \Nil (x :- y :- Nil) ->
              [idx x +: cst 1, idx y]
            -- f.pullback(g) = f ∘ g = [2*(i+1), 2*j]
            composed <- MA.pullback f g
            -- Build expected: [2*i + 2, 2*j]
            expected <- MA.mkMultiAff @'[] @2 @2 $ \Nil (x :- y :- Nil) ->
              [2 *: idx x +: cst 2, 2 *: idx y]
            -- Compare as maps
            m1 <- MA.toMap composed
            m2 <- MA.toMap expected
            (Ur result, _, _) <- Map.isEqual m1 m2
            Map.freeMap m1
            Map.freeMap m2
            return (Ur result)
      assertBool "pullback composition should be correct" eq

  , testCase "apply multi-aff to set" $ do
      let eq = runIslTest $ do
            -- Set: { [i, j] : 0 <= i <= 5, 0 <= j <= 5 }
            s <- Set.fromString @_ @'[] @2 "{ [i, j] : 0 <= i <= 5 and 0 <= j <= 5 }"
            -- Multi-aff: [i,j] -> [i+1, j]
            ma <- MA.mkMultiAff @'[] @2 @2 $ \Nil (x :- y :- Nil) ->
              [idx x +: cst 1, idx y]
            -- Apply
            result <- MA.apply ma s
            -- Expected: { [i, j] : 1 <= i <= 6, 0 <= j <= 5 }
            expected <- Set.fromString @_ @'[] @2 "{ [i, j] : 1 <= i <= 6 and 0 <= j <= 5 }"
            (Ur eq, _, _) <- Set.isEqual result expected
            Set.freeSet result
            Set.freeSet expected
            return (Ur eq)
      assertBool "apply should produce correct image" eq

  , testCase "decomposeMA roundtrip" $ do
      let eq = runIslTest $ do
            -- Build: [i,j] -> [i+j, i-j]
            ma <- MA.mkMultiAff @'[] @2 @2 $ \Nil (x :- y :- Nil) ->
              [idx x +: idx y, idx x -: idx y]
            -- Decompose
            (Ur exprs, ma') <- MA.decomposeMA ma
            -- Rebuild
            ma2 <- MA.toMultiAff @'[] @2 @2 exprs
            -- Compare as maps
            m1 <- MA.toMap ma'
            m2 <- MA.toMap ma2
            (Ur result, _, _) <- Map.isEqual m1 m2
            Map.freeMap m1
            Map.freeMap m2
            return (Ur result)
      assertBool "decompose → rebuild should preserve multi-aff" eq

  , testCase "fromString produces valid multi-aff" $ do
      let result = runIslTest $ do
            ma <- MA.fromString @_ @'[] @2 @2 "{ [i, j] -> [i + 1, j] }"
            (Ur str, ma') <- MA.borrowMA ma MA.maToString
            MA.freeMA ma'
            return (Ur str)
      assertBool "should contain arrow" ("->" `isInfixOfS` result)
  ]

isInfixOfS :: String -> String -> Bool
isInfixOfS needle haystack = any (isPrefixOfS needle) (tails haystack)
  where
    isPrefixOfS [] _ = True
    isPrefixOfS _ [] = False
    isPrefixOfS (a:as) (b:bs) = a == b && isPrefixOfS as bs
    tails [] = [[]]
    tails s@(_:xs) = s : tails xs
