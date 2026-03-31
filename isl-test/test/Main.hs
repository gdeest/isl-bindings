module Main where

import Test.Tasty

import qualified Test.BasicSet
import qualified Test.Set
import qualified Test.BasicMap
import qualified Test.Map
import qualified Test.Scan
import qualified Test.Multi
import qualified Test.MultiAff
import qualified Test.PwAff
import qualified Test.Div

main :: IO ()
main = defaultMain $ testGroup "ISL HighLevel Bindings"
  [ Test.BasicSet.tests
  , Test.Set.tests
  , Test.BasicMap.tests
  , Test.Map.tests
  , Test.Scan.tests
  , Test.Multi.tests
  , Test.MultiAff.tests
  , Test.PwAff.tests
  , Test.Div.tests
  ]
