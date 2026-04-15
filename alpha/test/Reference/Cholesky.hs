{-# LANGUAGE BangPatterns #-}

-- | Hand-written reference Cholesky factorisation, used by the test
-- harness as the ground truth for comparison against the Alpha-elaborated
-- kernel.  Parallels @Reference.Matmul@.
--
-- Given a symmetric positive-definite matrix @A@ (n×n, row-major), computes
-- the lower-triangular factor @L@ such that @L · L^T = A@.  The returned
-- vector has zeros strictly above the diagonal.
--
-- The algorithm follows the standard row-by-row definition:
--
-- * @L[i,i] = sqrt(A[i,i] - sum_{k=0..i-1} L[i,k]^2)@
-- * @L[i,j] = (A[i,j] - sum_{k=0..j-1} L[i,k] * L[j,k]) / L[j,j]@  for @j < i@
--
-- Use a mutable accumulator so the inductive reads of @L@ at rows already
-- written are straightforward.
module Reference.Cholesky (referenceCholesky) where

import Control.Monad (forM_)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

-- | Cholesky factorisation of an @n × n@ symmetric positive-definite matrix.
-- The input and output are stored row-major as flat vectors of length @n*n@.
-- The output has zeros strictly above the diagonal.
referenceCholesky
  :: Int            -- ^ N
  -> Vector Double  -- ^ A in row-major, symmetric positive-definite
  -> Vector Double  -- ^ L in row-major, lower-triangular
referenceCholesky n a = V.create $ do
  l <- MV.replicate (n * n) 0
  forM_ [0 .. n - 1] $ \i ->
    forM_ [0 .. i] $ \j -> do
      -- s = sum_{k=0..j-1} L[i,k] * L[j,k]
      s <- sumM [ (*) <$> MV.read l (i * n + k) <*> MV.read l (j * n + k)
                | k <- [0 .. j - 1]
                ]
      if i == j
        then
          -- L[i,i] = sqrt(A[i,i] - sum_{k=0..i-1} L[i,k]^2)
          -- Note that with i == j, the sum above already computes
          -- sum_{k=0..i-1} L[i,k] * L[i,k] = sum_{k=0..i-1} L[i,k]^2.
          MV.write l (i * n + j) (sqrt (a V.! (i * n + i) - s))
        else do
          -- L[i,j] = (A[i,j] - s) / L[j,j]
          ljj <- MV.read l (j * n + j)
          MV.write l (i * n + j) ((a V.! (i * n + j) - s) / ljj)
  return l

-- | Sum a list of monadic actions producing @Double@s, with a strict
-- accumulator so the inner products don't build a thunk list.
sumM :: Monad m => [m Double] -> m Double
sumM = go 0
  where
    go !acc []     = return acc
    go !acc (x:xs) = do
      v <- x
      go (acc + v) xs
