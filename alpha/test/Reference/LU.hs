{-# LANGUAGE BangPatterns #-}

-- | Hand-written reference Doolittle LU factorisation, used by the
-- test harness as the ground truth for comparison against the
-- Alpha-elaborated kernel.  Parallels
-- @Reference.{Matmul, Cholesky, FloydWarshall}@, but this is the
-- first reference in the suite that returns *two* output matrices
-- — L and U — as a pair of row-major vectors.
--
-- Doolittle convention: L is strict lower triangular with implicit
-- unit diagonal (i.e. @L[i,i] = 1@ is *not stored*), U is upper
-- triangular with the diagonal stored.  The returned @L@ therefore
-- has @L[i,i] = 0@ for every @i@ and zeros strictly above the
-- diagonal; @U@ has zeros strictly below the diagonal.  To recover
-- @A@ from the pair, one computes @(L + I) · U@ where @I@ is the
-- identity.
--
-- Standard update rules:
--
-- * @U[i, j] = A[i, j] - sum_{k=0..i-1} L[i, k] * U[k, j]@       for @i ≤ j@
-- * @L[i, j] = (A[i, j] - sum_{k=0..j-1} L[i, k] * U[k, j]) / U[j, j]@  for @j < i@
--
-- The outer loop walks rows @i = 0..N-1@.  Within each row we
-- compute @L[i, 0..i-1]@ *first* (each entry needs @U[j, j]@ and
-- earlier @L[i, k]@ entries, all from previous rows or earlier in
-- this row), then @U[i, i..N-1]@ (each entry needs @L[i, k]@ for
-- @k < i@ — now all written in this row's L phase).  The reverse
-- order is wrong: @U[1, 1]@ needs @L[1, 0]@, which wouldn't exist
-- yet if we did U first.
module Reference.LU (referenceLU) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

-- | Doolittle LU factorisation of an @n × n@ matrix.  The input is
-- row-major; the output is a pair @(L, U)@ where @L@ is strict
-- lower (unit diagonal *not* stored, zeros on and above the
-- diagonal) and @U@ is upper with the diagonal stored.
referenceLU
  :: Int            -- ^ N
  -> Vector Double  -- ^ A in row-major
  -> (Vector Double, Vector Double)  -- ^ (L, U) in row-major
referenceLU n a = runST $ do
  l <- MV.replicate (n * n) 0
  u <- MV.replicate (n * n) 0
  forM_ [0 .. n - 1] $ \i -> do
    -- L[i, j] for j = 0..i-1 (strict lower) — must come first so
    -- that the U[i, ...] updates below can read the freshly
    -- written L[i, k] entries.
    forM_ [0 .. i - 1] $ \j -> do
      s <- sumM [ (*) <$> MV.read l (i * n + k) <*> MV.read u (k * n + j)
                | k <- [0 .. j - 1]
                ]
      ujj <- MV.read u (j * n + j)
      MV.write l (i * n + j) ((a V.! (i * n + j) - s) / ujj)
    -- U[i, j] for j = i..n-1 (upper including diagonal).
    forM_ [i .. n - 1] $ \j -> do
      s <- sumM [ (*) <$> MV.read l (i * n + k) <*> MV.read u (k * n + j)
                | k <- [0 .. i - 1]
                ]
      MV.write u (i * n + j) (a V.! (i * n + j) - s)
  l' <- V.unsafeFreeze l
  u' <- V.unsafeFreeze u
  return (l', u')

-- | Strict monadic sum.
sumM :: Monad m => [m Double] -> m Double
sumM = go 0
  where
    go !acc []     = return acc
    go !acc (x:xs) = x >>= \v -> go (acc + v) xs
