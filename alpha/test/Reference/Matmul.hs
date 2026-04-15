-- | Hand-written reference matmul, used by the test harness as the
-- ground truth for comparison against the Alpha-elaborated kernel.
-- Until codegen-to-C lands in v6, the test harness only inspects the
-- elaborated core's structure (the matmul example just has to compile
-- and pattern-match), but this reference is here so the test file is
-- ready when the codegen path comes online.
module Reference.Matmul (referenceMatmul) where

import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)

-- | The classical N×N matrix multiply: @C[i, j] = sum_k A[i, k] * B[k, j]@.
-- Inputs and the output are stored row-major as flat vectors of length @n*n@.
-- The function allocates a fresh result vector.
referenceMatmul
  :: Int          -- ^ N
  -> Vector Double  -- ^ A in row-major
  -> Vector Double  -- ^ B in row-major
  -> Vector Double
referenceMatmul n a b = V.generate (n * n) $ \ij ->
  let i = ij `div` n
      j = ij `mod` n
   in sum [ a V.! (i * n + k) * b V.! (k * n + j) | k <- [0 .. n - 1] ]
