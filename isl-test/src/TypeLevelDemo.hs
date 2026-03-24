{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}

-- | Demo: type-level polyhedral constraints verified at compile time by ISL.
module Main where

import Isl.TypeLevel
import Isl.HighLevel.Pure (PConjunction(..))

-- | A triangular iteration domain:
--   { (i, j) : 0 ≤ i ≤ N, 0 ≤ j ≤ i }
type Triangle =
  '[ 'TDim 0  >=. 'TConst ('Pos 0)         -- i ≥ 0
   , 'TParam "N" >=. 'TDim 0                -- i ≤ N
   , 'TDim 1  >=. 'TConst ('Pos 0)          -- j ≥ 0
   , 'TDim 0  >=. 'TDim 1                   -- j ≤ i
   ]

-- | A rectangular domain that contains the triangle:
--   { (i, j) : 0 ≤ i ≤ N, 0 ≤ j ≤ N }
type Rectangle =
  '[ 'TDim 0  >=. 'TConst ('Pos 0)
   , 'TParam "N" >=. 'TDim 0
   , 'TDim 1  >=. 'TConst ('Pos 0)
   , 'TParam "N" >=. 'TDim 1
   ]

-- | ISL proves at compile time: Triangle ⊆ Rectangle.
-- If this relationship didn't hold, the module would fail to compile!
proofSubset :: IslSubset '["N"] 2 Triangle Rectangle => ()
proofSubset = ()

-- | Reify type-level sets to runtime (for display)
instance ParamIndex "N" where paramIndex = 0

triangleValue :: PConjunction '["N"] 2
triangleValue = reifyBasicSet @'["N"] @2 @Triangle

rectangleValue :: PConjunction '["N"] 2
rectangleValue = reifyBasicSet @'["N"] @2 @Rectangle

main :: IO ()
main = do
  putStrLn "=== Type-Level Polyhedral Proofs ==="
  putStrLn ""
  putStrLn "Triangle:"
  print triangleValue
  putStrLn ""
  putStrLn "Rectangle:"
  print rectangleValue
  putStrLn ""
  putStrLn "Triangle ⊆ Rectangle: proved at compile time!"
  putStrLn (show proofSubset)
