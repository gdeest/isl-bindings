module Main where

import Test.Tasty
import Test.Tasty.HUnit (testCase, Assertion)
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

import Isl.LeakCheck (assertNoIslLeaks)

-- | Wrap the whole suite under a leak checker so every 'runIslT' is
-- observed. fd-2 redirection is serialized by the module-level lock
-- inside 'Isl.LeakCheck', so parallel tasty execution stays correct.
leakChecked :: TestName -> Assertion -> TestTree
leakChecked name act = testCase name (assertNoIslLeaks name act)

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
  , leakCheckSelf
  ]

-- | A minimal positive test that exercises the leak-check harness itself.
leakCheckSelf :: TestTree
leakCheckSelf = testGroup "LeakCheck self-test"
  [ leakChecked "captures nothing on clean run" (pure ())
  ]
