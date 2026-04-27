-- | Hand-written reference matmul, used by the test harness as the
-- ground truth for comparison against the Alpha-elaborated kernel.
module Reference.Matmul (referenceMatmul) where

import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)

-- | The classical N×N matrix multiply: @C[i, j] = sum_k A[i, k] * B[k, j]@.
-- Inputs and the output are stored row-major as flat vectors of length @n*n@.
-- The function allocates a fresh result vector.
referenceMatmul
  :: (Num a, V.Unbox a)
  => Int -> Vector a -> Vector a -> Vector a
referenceMatmul n a b = V.generate (n * n) $ \ij ->
  let i = ij `div` n
      j = ij `mod` n
   in sum [ a V.! (i * n + k) * b V.! (k * n + j) | k <- [0 .. n - 1] ]
