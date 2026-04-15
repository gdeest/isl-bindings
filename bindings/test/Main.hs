module Main where

import Test.Tasty
import qualified Test.RawBasicSet
import qualified Test.RawSet
import qualified Test.RawBasicMap
import qualified Test.RawMap
import qualified Test.RawMultiAff
import qualified Test.RawPwAff
import qualified Test.RawUnionMap
import qualified Test.RawFlowAnalysis
import qualified Test.TypedConstraints
import qualified Test.TypedBuild

main :: IO ()
main = defaultMain $ testGroup "ISL Bindings"
  [ Test.RawBasicSet.tests
  , Test.RawSet.tests
  , Test.RawBasicMap.tests
  , Test.RawMap.tests
  , Test.RawMultiAff.tests
  , Test.RawPwAff.tests
  , Test.RawUnionMap.tests
  , Test.RawFlowAnalysis.tests
  , Test.TypedConstraints.tests
  , Test.TypedBuild.tests
  ]
