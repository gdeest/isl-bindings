{-# LANGUAGE BangPatterns #-}

-- | Hand-written reference 3-D heat equation stencil, used by the
-- test harness as the ground truth for comparison against the
-- Alpha-elaborated kernel.  Parallels
-- @Reference.{Matmul, Cholesky, FloydWarshall, LU}@.
--
-- The stencil is the simplest six-neighbour averaging update:
--
-- @
-- u[t, i, j, k] = (u[t-1, i-1, j, k] + u[t-1, i+1, j, k]
--               + u[t-1, i, j-1, k] + u[t-1, i, j+1, k]
--               + u[t-1, i, j, k-1] + u[t-1, i, j, k+1]) / 6
-- @
--
-- (for interior points at @t ≥ 1@; boundary points at @t ≥ 1@ are
-- set to @0@, and the @t = 0@ slice copies @u0@).  The discrete
-- Laplacian of a linear-in-(i, j, k) function is zero under this
-- averaging, so a uniform or linear initial condition is preserved
-- on the interior across time steps — which is the basis for the
-- hand-verified test case below.
module Reference.Heat3D (referenceHeat3D) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

-- | Reference 3-D heat equation solver.  Input @u0@ is an N×N×N
-- initial condition in row-major @(i, j, k)@ order; output is a
-- T×N×N×N space-time array in row-major @(t, i, j, k)@ order.
referenceHeat3D
  :: Int            -- ^ N (spatial grid size)
  -> Int            -- ^ T (number of time steps)
  -> Vector Double  -- ^ u0 in row-major, length N*N*N
  -> Vector Double  -- ^ u in row-major, length T*N*N*N
referenceHeat3D n nt u0 = runST $ do
  u <- MV.replicate (nt * n * n * n) 0
  -- t = 0 slice: copy u0 into every (0, i, j, k).
  forM_ [0 .. n - 1] $ \i ->
    forM_ [0 .. n - 1] $ \j ->
      forM_ [0 .. n - 1] $ \k ->
        MV.write u (idx4 n 0 i j k) (u0 V.! idx3 n i j k)
  -- t = 1..T-1: stencil update with zero Dirichlet boundary.
  forM_ [1 .. nt - 1] $ \t ->
    forM_ [0 .. n - 1] $ \i ->
      forM_ [0 .. n - 1] $ \j ->
        forM_ [0 .. n - 1] $ \k -> do
          if i == 0 || i == n - 1 || j == 0 || j == n - 1 || k == 0 || k == n - 1
            then MV.write u (idx4 n t i j k) 0
            else do
              -- Six neighbour reads at t - 1.
              im <- MV.read u (idx4 n (t - 1) (i - 1) j k)
              ip <- MV.read u (idx4 n (t - 1) (i + 1) j k)
              jm <- MV.read u (idx4 n (t - 1) i (j - 1) k)
              jp <- MV.read u (idx4 n (t - 1) i (j + 1) k)
              km <- MV.read u (idx4 n (t - 1) i j (k - 1))
              kp <- MV.read u (idx4 n (t - 1) i j (k + 1))
              let !avg = (im + ip + jm + jp + km + kp) / 6
              MV.write u (idx4 n t i j k) avg
  V.unsafeFreeze u

-- | Row-major index into a 4-D @T × N × N × N@ array.
idx4 :: Int -> Int -> Int -> Int -> Int -> Int
idx4 n t i j k = t * n * n * n + i * n * n + j * n + k

-- | Row-major index into a 3-D @N × N × N@ array.
idx3 :: Int -> Int -> Int -> Int -> Int
idx3 n i j k = i * n * n + j * n + k
