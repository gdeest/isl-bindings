{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude hiding (min, max)

import Isl.HighLevel.Context
import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.BasicSet (BasicSet)
import qualified Isl.HighLevel.BasicSet as BS
import qualified Isl.Linear as Isl

-- Modulo is implemented with an existential variable, by projecting out
-- the last dimension of the set.
evenXs :: Isl (BasicSet 2)
evenXs = Isl.do
  bs3 <- BS.mkBasicSet @3 $
    \(x :- _ :- k :- Nil) -> toConjunction $
      idx x ==: 2 *: idx k
  BS.eliminateLast bs3

someSet :: Isl (BasicSet 2)
someSet = BS.mkBasicSet @2 $
  \(x :- y :- Nil) ->
    idx x >=: cst 0 &&: idx x <=: cst 100 &&:
    idx y >=: idx x &&: idx y <=: cst 100

main :: IO ()
main = do
  let (s1, s2, s3) = runIsl $ Isl.do
        ex <- evenXs
        ss <- someSet
        (Ur s1, ex') <- BS.borrowBS ex BS.bsetToString
        (Ur s2, ss') <- BS.borrowBS ss BS.bsetToString
        isect <- BS.intersect ex' ss'
        (Ur s3, isect') <- BS.borrowBS isect BS.bsetToString
        BS.freeBS isect'
        Isl.pure (Ur (s1, s2, s3))
  putStrLn s1
  putStrLn s2
  putStrLn s3
