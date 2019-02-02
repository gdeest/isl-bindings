module Main where

import Data.Reflection
import Isl.Types
import qualified Isl.BasicMap.AutoGen as BMap

main :: IO ()
main = do
  ctx <- ctx_alloc
  give ctx $ do
    let bMap = BMap.readFromStr  "{[i,j] -> [z]: i+j <= z && i >= 0}"
        bMap1 = BMap.readFromStr "{[i,j] -> [z]: exists y: z = 2*y}"
    putStrLn $ "#constraints: "
    print $ BMap.nConstraint bMap
    print $ BMap.nConstraint $ BMap.intersect bMap bMap1
