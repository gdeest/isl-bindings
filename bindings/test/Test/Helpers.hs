module Test.Helpers (runIslTest) where

import Control.DeepSeq (NFData)
import Isl.Monad (Isl, Ur(..), runIsl)

runIslTest :: NFData a => Isl (Ur a) -> a
runIslTest = runIsl
