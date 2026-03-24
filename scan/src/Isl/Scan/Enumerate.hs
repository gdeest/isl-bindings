{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

-- | Enumerate integer points of polyhedra using compiled scanners.
module Isl.Scan.Enumerate
  ( scanPoints
  , scanFold
  , scanForM_
  , scanMapM
  , evalBound
  , evalLower
  , evalUpper
  , ceilDiv
  , floorDiv
  ) where

import GHC.TypeLits (Symbol, Nat)

import Isl.HighLevel.Params (Length)
import Isl.Scan.Types

-- | Enumerate all integer points in a (possibly non-convex) set.
--
-- @params@ is a 'Vec' of parameter values, indexed positionally
-- (matching the parameter order in the ISL space).
--
-- Returns a list of points, where each point is a 'Vec' of dimension
-- values (outermost first).
--
-- For non-convex sets (multiple loop nests), results are concatenated.
-- If the scanner was built from a disjoint decomposition, there are
-- no duplicates.
scanPoints :: Scanner ps n -> Vec (Length ps) Integer -> [Vec n Integer]
scanPoints (Scanner nests) params =
  concatMap (\nest -> scanLoopNest nest (toList params)) nests

-- | Strict left fold over all integer points.
--
-- More efficient than @foldl' f z (scanPoints s params)@ because
-- it avoids building the intermediate list of points.
scanFold :: Scanner ps n -> Vec (Length ps) Integer -> (a -> Vec n Integer -> a) -> a -> a
scanFold (Scanner nests) params f z =
  foldl (\acc nest -> foldLoopNest nest (toList params) (\a pt -> f a (unsafeVec pt)) acc) z nests

-- | Monadic traversal over all integer points.
scanForM_ :: Monad m => Scanner ps n -> Vec (Length ps) Integer -> (Vec n Integer -> m ()) -> m ()
scanForM_ scanner params action =
  scanFold scanner params (\m pt -> m >> action pt) (return ())

-- | Monadic map over all integer points.
scanMapM :: Monad m => Scanner ps n -> Vec (Length ps) Integer -> (Vec n Integer -> m a) -> m [a]
scanMapM scanner params action =
  fmap reverse $ scanFold scanner params (\m pt -> m >>= \acc -> action pt >>= \a -> return (a : acc)) (return [])

-- | Enumerate all integer points of a single convex loop nest.
scanLoopNest :: LoopNest ps n -> [Integer] -> [Vec n Integer]
scanLoopNest nest params = go [] (lnLevels nest)
  where
    go prefix [] = [unsafeVec (reverse prefix)]
    go prefix (level : rest) =
      case llEquality level of
        Just eq ->
          -- Equality constraint: value is determined
          let val = evalBoundExact params prefix eq
          in go (val : prefix) rest
        Nothing ->
          -- Range: iterate from max(lower bounds) to min(upper bounds)
          let lo = maximum' $ map (evalLower params prefix) (llLowerBounds level)
              hi = minimum' $ map (evalUpper params prefix) (llUpperBounds level)
              stride = llStride level
          in concatMap (\v -> go (v : prefix) rest)
                       [lo, lo + stride .. hi]

-- | Strict fold over a single loop nest.
foldLoopNest :: LoopNest ps n -> [Integer] -> (a -> [Integer] -> a) -> a -> a
foldLoopNest nest params f = go [] (lnLevels nest)
  where
    go prefix [] acc = f acc (reverse prefix)
    go prefix (level : rest) acc =
      case llEquality level of
        Just eq ->
          let val = evalBoundExact params prefix eq
          in go (val : prefix) rest acc
        Nothing ->
          let lo = maximum' $ map (evalLower params prefix) (llLowerBounds level)
              hi = minimum' $ map (evalUpper params prefix) (llUpperBounds level)
              stride = llStride level
          in foldl (\a v -> go (v : prefix) rest a) acc [lo, lo + stride .. hi]

-- | Evaluate an affine bound given parameter values and outer loop values.
--
-- @prefix@ is the list of outer loop variable values, most-recently-assigned
-- first (i.e. @prefix !! 0@ is the immediately enclosing loop).
-- This matches the build order where dimension 0 is outermost.
evalBound :: [Integer] -> [Integer] -> AffineBound -> Integer
evalBound params prefix (AffineBound loopCs paramCs constant _divisor) =
  let loopSum = sum [c * indexLoop prefix i | (c, i) <- loopCs]
      paramSum = sum [c * indexParam params i | (c, i) <- paramCs]
  in loopSum + paramSum + constant

-- | Evaluate a lower bound: ceil(bound / divisor).
evalLower :: [Integer] -> [Integer] -> AffineBound -> Integer
evalLower params prefix bound =
  let num = evalBound params prefix bound
  in ceilDiv num (abDivisor bound)

-- | Evaluate an upper bound: floor(bound / divisor).
evalUpper :: [Integer] -> [Integer] -> AffineBound -> Integer
evalUpper params prefix bound =
  let num = evalBound params prefix bound
  in floorDiv num (abDivisor bound)

-- | Evaluate an equality bound (must divide exactly, or we round).
evalBoundExact :: [Integer] -> [Integer] -> AffineBound -> Integer
evalBoundExact params prefix bound =
  let num = evalBound params prefix bound
  in num `div` abDivisor bound

-- | Index into the loop variable prefix.
-- Dimension 0 is outermost; prefix is stored outermost-first after reversal,
-- but during construction it's most-recent-first. We index by dimension number.
indexLoop :: [Integer] -> Int -> Integer
indexLoop prefix i =
  -- prefix is built innermost-first (most recent assignment first),
  -- but dimensions are numbered outermost-first.
  -- So dimension i corresponds to prefix !! (length prefix - 1 - i)
  let idx = length prefix - 1 - i
  in if idx >= 0 && idx < length prefix
     then prefix !! idx
     else error $ "indexLoop: dimension " ++ show i ++ " not yet assigned (prefix length " ++ show (length prefix) ++ ")"

indexParam :: [Integer] -> Int -> Integer
indexParam params i
  | i >= 0 && i < length params = params !! i
  | otherwise = error $ "indexParam: parameter " ++ show i ++ " out of range"

-- | Ceiling division: smallest integer >= a/b (b > 0).
ceilDiv :: Integer -> Integer -> Integer
ceilDiv a b
  | b <= 0    = error "ceilDiv: non-positive divisor"
  | a >= 0    = (a + b - 1) `div` b
  | otherwise = -((-a) `div` b)

-- | Floor division: largest integer <= a/b (b > 0).
floorDiv :: Integer -> Integer -> Integer
floorDiv a b
  | b <= 0    = error "floorDiv: non-positive divisor"
  | a >= 0    = a `div` b
  | otherwise = -(((-a) + b - 1) `div` b)

-- | Maximum of a non-empty list of bounds.
-- An empty list means the dimension is unconstrained from below — this
-- is an error for bounded scanning.
maximum' :: [Integer] -> Integer
maximum' [] = error "maximum': no lower bounds (unbounded dimension)"
maximum' xs = maximum xs

-- | Minimum of a non-empty list of bounds.
minimum' :: [Integer] -> Integer
minimum' [] = error "minimum': no upper bounds (unbounded dimension)"
minimum' xs = minimum xs
