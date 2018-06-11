module Main where

import Data.Reflection
import Isl.Types
import qualified Isl.BasicMap.AutoGen as BMap

main :: IO ()
main = do
  ctx <- ctx_alloc
  give ctx $ do
    let bMap = BMap.readFromStr ctx "{[i,j] -> [z]: i+j <= z && i >= 0}"
    putStrLn $ "#constraints: "
    print $ BMap.nConstraint bMap
