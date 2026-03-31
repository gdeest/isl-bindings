{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Div (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Isl.Monad (Ur(..))
import Isl.HighLevel.Constraints
import Isl.HighLevel.Pure (PConjunction(..), PDisjunction(..), PMapConjunction(..), PMapDisjunction(..))

import qualified Isl.HighLevel.BasicSet as BS
import qualified Isl.HighLevel.BasicMap as BM
import qualified Isl.HighLevel.Set as Set
import qualified Isl.HighLevel.Map as Map

import Test.Helpers

tests :: TestTree
tests = testGroup "Div (existential variables)"
  [ setDivTests
  , mapDivTests
  ]

setDivTests :: TestTree
setDivTests = testGroup "Set"
  [ testCase "fromString with div, decompose preserves FloorDiv" $ do
      -- { [i] : exists k: i = 3k and 0 <= i <= 9 }
      -- This is the set {0, 3, 6, 9}
      let result = runIslTest $ do
            bs <- BS.fromString @_ @'[] @1 "{ [i] : exists k: i = 3k and 0 <= i <= 9 }"
            (Ur (PConjunction (Conjunction cs)), bs') <- BS.decomposeBS bs
            BS.freeBS bs'
            return (Ur cs)
      -- The decomposed constraints should contain FloorDiv
      assertBool "decomposed constraints should contain FloorDiv"
        (conjunctionHasFloorDiv result)

  , testCase "single div: decompose → rebuild round-trip" $ do
      -- Build { [i] : exists k: i = 3k and 0 <= i <= 9 }
      -- Decompose, rebuild, check ISL equality
      let eq = runIslTest $ do
            original <- BS.fromString @_ @'[] @1 "{ [i] : exists k: i = 3k and 0 <= i <= 9 }"
            s1 <- Set.fromBasicSet original
            (Ur (PDisjunction conjs), s1') <- Set.decomposeSet s1
            -- Rebuild from decomposed constraints
            s2 <- case conjs of
              [PConjunction c] -> mkSet @'[] @1 c
              cs -> do
                sets <- mapM (\(PConjunction c) -> mkSet @'[] @1 c) cs
                case sets of
                  (h:t) -> foldl (\acc s -> do a <- acc; Set.union a s) (return h) t
                  []    -> error "empty disjunction"
            (Ur result, s1'', s2') <- Set.isEqual s1' s2
            Set.freeSet s1''
            Set.freeSet s2'
            return (Ur result)
      assertBool "div round-trip should preserve set" eq

  , testCase "complement of div set: decompose → rebuild round-trip" $ do
      -- Complement of a modular set produces more complex div structure
      let eq = runIslTest $ do
            bs <- BS.fromString @_ @'[] @1 "{ [i] : exists k: i = 3k and 0 <= i <= 9 }"
            s1 <- Set.fromBasicSet bs
            compl <- Set.complement s1
            -- Decompose complement (may have nested divs)
            (Ur (PDisjunction conjs), compl') <- Set.decomposeSet compl
            -- Rebuild
            rebuilt <- case conjs of
              [] -> error "empty complement"
              [PConjunction c] -> mkSet @'[] @1 c
              cs -> do
                sets <- mapM (\(PConjunction c) -> mkSet @'[] @1 c) cs
                case sets of
                  (h:t) -> foldl (\acc s -> do a <- acc; Set.union a s) (return h) t
                  []    -> error "empty disjunction"
            (Ur result, compl'', rebuilt') <- Set.isEqual compl' rebuilt
            Set.freeSet compl''
            Set.freeSet rebuilt'
            return (Ur result)
      assertBool "complement div round-trip should preserve set" eq

  , testCase "nested div: floor(floor(i/2)/3) round-trip" $ do
      -- { [i] : exists a, b: a = floor(i/2) and b = floor(a/3) and 0 <= i <= 100 and b >= 2 }
      -- This encodes floor(floor(i/2)/3) >= 2, so i >= 12
      let eq = runIslTest $ do
            original <- BS.fromString @_ @'[] @1
              "{ [i] : exists a, b: a = floor(i/2) and b = floor(a/3) and 0 <= i <= 100 and b >= 2 }"
            s1 <- Set.fromBasicSet original
            (Ur (PDisjunction conjs), s1') <- Set.decomposeSet s1
            rebuilt <- case conjs of
              [] -> error "empty"
              [PConjunction c] -> mkSet @'[] @1 c
              cs -> do
                sets <- mapM (\(PConjunction c) -> mkSet @'[] @1 c) cs
                case sets of
                  (h:t) -> foldl (\acc s -> do a <- acc; Set.union a s) (return h) t
                  []    -> error "empty disjunction"
            (Ur result, s1'', rebuilt') <- Set.isEqual s1' rebuilt
            Set.freeSet s1''
            Set.freeSet rebuilt'
            return (Ur result)
      assertBool "nested div round-trip should preserve set" eq

  , testCase "parametric div: decompose → rebuild round-trip" $ do
      -- { [i] : exists k: i = 3k and 0 <= i <= N } with param N
      let eq = runIslTest $ do
            original <- BS.fromString @_ @'["N"] @1
              "[N] -> { [i] : exists k: i = 3k and 0 <= i <= N }"
            s1 <- Set.fromBasicSet original
            (Ur (PDisjunction conjs), s1') <- Set.decomposeSet s1
            rebuilt <- case conjs of
              [PConjunction c] -> mkSet @'["N"] @1 c
              cs -> do
                sets <- mapM (\(PConjunction c) -> mkSet @'["N"] @1 c) cs
                case sets of
                  (h:t) -> foldl (\acc s -> do a <- acc; Set.union a s) (return h) t
                  []    -> error "empty disjunction"
            (Ur result, s1'', rebuilt') <- Set.isEqual s1' rebuilt
            Set.freeSet s1''
            Set.freeSet rebuilt'
            return (Ur result)
      assertBool "parametric div round-trip should preserve set" eq
  ]

mapDivTests :: TestTree
mapDivTests = testGroup "Map"
  [ testCase "map with div: decompose → rebuild round-trip" $ do
      -- { [i] -> [j] : j = floor(i/2) and 0 <= i <= 10 }
      let eq = runIslTest $ do
            original <- BM.fromString @_ @'[] @1 @1
              "{ [i] -> [j] : j = floor(i/2) and 0 <= i <= 10 }"
            m1 <- Map.fromBasicMap original
            (Ur (PMapDisjunction conjs), m1') <- Map.decomposeMap m1
            rebuilt <- case conjs of
              [PMapConjunction c] -> mkMap @'[] @1 @1 c
              cs -> do
                maps <- mapM (\(PMapConjunction c) -> mkMap @'[] @1 @1 c) cs
                case maps of
                  (h:t) -> foldl (\acc m -> do a <- acc; Map.union a m) (return h) t
                  []    -> error "empty"
            (Ur result, m1'', rebuilt') <- Map.isEqual m1' rebuilt
            Map.freeMap m1''
            Map.freeMap rebuilt'
            return (Ur result)
      assertBool "map div round-trip should preserve map" eq
  ]
