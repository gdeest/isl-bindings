{-# LANGUAGE BangPatterns #-}

-- | Hand-written reference Floyd-Warshall all-pairs shortest path,
-- used by the test harness as the ground truth for comparison
-- against the Alpha-elaborated kernel.  Parallels
-- @Reference.Matmul@ / @Reference.Cholesky@.
--
-- Given an @n × n@ adjacency matrix @A@ (row-major, with +∞ for
-- missing edges), computes the all-pairs shortest-path matrix
-- @Result@ such that @Result[i, j]@ is the minimum total weight of
-- a path from @i@ to @j@.
--
-- Standard Floyd-Warshall iteration:
--
--   D[-1, i, j] = A[i, j]
--   D[k,  i, j] = min(D[k-1, i, j], D[k-1, i, k] + D[k-1, k, j])
--   Result[i, j] = D[N-1, i, j]
--
-- The reference implementation fuses the three-dimensional @D@
-- into a single 2D in-place matrix, which is the standard
-- optimisation.  The intermediate 3D structure exists only in the
-- Alpha encoding (where it's the declared domain of the @D@ local
-- variable) so that per-@k@ reads at @D[k-1, …]@ are well-typed.
module Reference.FloydWarshall (referenceFloydWarshall) where

import Control.Monad (forM_)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

-- | Floyd-Warshall on an @n × n@ adjacency matrix.  The input matrix
-- is row-major, with missing edges encoded as a large finite weight
-- (we use @1 / 0 = +Infinity@).  The output is row-major as well.
referenceFloydWarshall
  :: Int            -- ^ N
  -> Vector Double  -- ^ A in row-major
  -> Vector Double  -- ^ Result in row-major
referenceFloydWarshall n a = V.create $ do
  -- Working copy of A that we update in place through the
  -- Floyd-Warshall iteration.
  d <- MV.replicate (n * n) 0
  forM_ [0 .. n * n - 1] $ \idx -> MV.write d idx (a V.! idx)
  forM_ [0 .. n - 1] $ \k ->
    forM_ [0 .. n - 1] $ \i ->
      forM_ [0 .. n - 1] $ \j -> do
        dij <- MV.read d (i * n + j)
        dik <- MV.read d (i * n + k)
        dkj <- MV.read d (k * n + j)
        let !candidate = dik + dkj
        MV.write d (i * n + j) (min dij candidate)
  return d
