{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude hiding (min, max)

import Data.Proxy

import Isl.HighLevel.Context
import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.BasicSet (BasicSet)
import qualified Isl.HighLevel.BasicSet as BS

-- Modulo is implemented with a existential variable, by projecting out
-- the last dimension of the set.
evenXs :: forall s. HasCtx s => BasicSet s 2
evenXs = BS.eliminateLast $ BS.mkBasicSet $
  \(x :- _ :- k :- Nil) -> toConjunction $
    idx x ==: 2 *: idx k

someSet :: forall s. HasCtx s => BasicSet s 2
someSet = BS.mkBasicSet $
  \(x :- y :- Nil) ->
    idx x >=: cst 0 &&: idx x <=: cst 100 &&:
    idx y >=: idx x &&: idx y <=: cst 100

myTest :: forall s. HasCtx s => Proxy s -> IO ()
myTest _ = do
  let pp = putStrLn . BS.bsetToString
  pp $ evenXs @s
  pp $ someSet @s
  pp $ evenXs @s `BS.intersect` someSet @s

main :: IO ()
main = runIsl myTest
