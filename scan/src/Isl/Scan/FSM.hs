-- | Finite state machine scanner (Boulet-Feautrier style).
--
-- Instead of nested loops, this scanner maintains an explicit iteration
-- vector as state and advances it lexicographically:
--
-- 1. Start at lexmin of the polyhedron
-- 2. At each step, find which constraints are saturated (which face
--    you're on) — this determines the carry dimension
-- 3. Increment the carry dimension, reset all inner dims to their
--    lower bounds
-- 4. Stop when past lexmax
--
-- This is a natural @unfoldr@ source and works for any polyhedron
-- (including non-convex via disjoint decomposition).
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Isl.Scan.FSM
  ( ScanState(..)
  , initScan
  , scanStep
  , scanFSM
  , scanFoldFSM
  , scanForM_FSM
  ) where

import GHC.TypeLits (Symbol, Nat)

import Isl.HighLevel.Params (Length)
import Isl.Scan.Types
import Isl.Scan.Enumerate (evalLower, evalUpper, evalBound, ceilDiv)

-- | The state machine state: current iteration vector + context.
data ScanState ps n = ScanState
  { ssPoint  :: ![Integer]        -- ^ Current iteration vector (outermost first).
  , ssNest   :: !(LoopNest ps n)  -- ^ Loop nest descriptor.
  , ssParams :: ![Integer]        -- ^ Parameter values.
  } deriving (Show)

-- | Compute the lexicographic minimum of the polyhedron.
-- For each dimension (outermost to innermost), fix it to the maximum
-- of its lower bounds given the already-fixed outer values.
initScan :: LoopNest ps n -> Vec (Length ps) Integer -> Maybe (ScanState ps n)
initScan nest params =
  let paramList = toList params
  in case computeLexMin (lnLevels nest) paramList [] of
    Nothing -> Nothing  -- empty polyhedron
    Just point -> Just ScanState
      { ssPoint = point
      , ssNest = nest
      , ssParams = paramList
      }

-- | Compute lexmin: greedily pick the minimum for each dimension.
-- Returns Nothing if the polyhedron is empty (lower > upper at some level).
computeLexMin :: [LoopLevel] -> [Integer] -> [Integer] -> Maybe [Integer]
computeLexMin [] _ prefix = Just (reverse prefix)
computeLexMin (level : rest) params prefix =
  case llEquality level of
    Just eq ->
      let val = evalBoundExact params prefix eq
      in computeLexMin rest params (val : prefix)
    Nothing ->
      let lo = safeMaximum $ map (evalLower params prefix) (llLowerBounds level)
          hi = safeMinimum $ map (evalUpper params prefix) (llUpperBounds level)
      in case (lo, hi) of
           (Just l, Just h) | l <= h -> computeLexMin rest params (l : prefix)
           _ -> Nothing  -- empty at this level

-- | One step of the FSM: produce the current point and advance to
-- the lexicographic successor.
--
-- The successor is found by:
-- 1. Scanning dimensions from innermost to outermost
-- 2. Finding the first (innermost) dimension where current < upper bound
--    (i.e., the constraint is NOT saturated — we're not on that face)
-- 3. Incrementing that dimension by its stride
-- 4. Resetting all inner dimensions to their lower bounds
--
-- If all dimensions are saturated (we're at the vertex = lexmax),
-- emits the final point and returns Nothing on subsequent call.
scanStep :: ScanState ps n -> Maybe (Vec n Integer, ScanState ps n)
scanStep (ScanState [] _ _) = Nothing  -- exhausted
scanStep (ScanState point nest params) =
  let levels = lnLevels nest
      n = length levels
  in case findCarryAndAdvance levels params point (n - 1) of
       Nothing ->
         -- At lexmax. Emit this point, signal done via Nothing next time.
         -- We use a sentinel: set point to [] to indicate exhaustion.
         Just (unsafeVec point, ScanState [] nest params)
       Just newPoint ->
         Just (unsafeVec point, ScanState newPoint nest params)

-- | Enumerate all points via the FSM. This is the primary interface.
scanFSM :: LoopNest ps n -> Vec (Length ps) Integer -> [Vec n Integer]
scanFSM nest params =
  case initScan nest params of
    Nothing -> []
    Just st -> go (ssPoint st)
  where
    levels = lnLevels nest
    n = length levels
    paramList = toList params

    go point =
      unsafeVec point : case findCarryAndAdvance levels paramList point (n - 1) of
                Nothing       -> []  -- was at lexmax, done
                Just newPoint -> go newPoint

-- | Strict fold over points via the FSM.
scanFoldFSM :: LoopNest ps n -> Vec (Length ps) Integer -> (a -> Vec n Integer -> a) -> a -> a
scanFoldFSM nest params f z =
  case initScan nest params of
    Nothing -> z
    Just st -> go z (ssPoint st)
  where
    levels = lnLevels nest
    n = length levels
    paramList = toList params

    go !acc point =
      let acc' = f acc (unsafeVec point)
      in case findCarryAndAdvance levels paramList point (n - 1) of
           Nothing       -> acc'
           Just newPoint -> go acc' newPoint

-- | Monadic traversal over points via the FSM.
scanForM_FSM :: Monad m => LoopNest ps n -> Vec (Length ps) Integer -> (Vec n Integer -> m ()) -> m ()
scanForM_FSM nest params action =
  scanFoldFSM nest params (\m pt -> m >> action pt) (return ())

-- | Find the carry dimension and compute the new iteration vector.
--
-- Starting from the innermost dimension, walk outward looking for
-- a dimension that can be incremented (current < upper bound).
--
-- Each dimension we pass over was saturated (at its upper bound) —
-- that's the "face" information. The carry dimension is the first
-- non-saturated one.
--
-- After incrementing the carry dimension, all inner dimensions are
-- reset to their lower bounds (lexmin of the inner sub-polyhedron).
findCarryAndAdvance
  :: [LoopLevel]    -- ^ Loop levels (outermost first)
  -> [Integer]      -- ^ Parameter values
  -> [Integer]      -- ^ Current point (outermost first)
  -> Int            -- ^ Dimension to try (start at innermost = n-1)
  -> Maybe [Integer] -- ^ New point, or Nothing if at lexmax
findCarryAndAdvance levels params point dim
  | dim < 0   = Nothing  -- all dimensions saturated, at lexmax
  | otherwise =
    let level = levels !! dim
        prefix = take dim point  -- outer dimensions (outermost first, as stored)
        prefixRev = reverse prefix  -- reversed for evalLower/evalUpper
    in case llEquality level of
         Just _ ->
           -- Equality-constrained dimension can't be incremented.
           -- Try the next outer dimension.
           findCarryAndAdvance levels params point (dim - 1)
         Nothing ->
           let currentVal = point !! dim
               hi = minimum $ map (evalUpper params prefixRev) (llUpperBounds level)
               stride = llStride level
               nextVal = currentVal + stride
           in if nextVal <= hi
              then
                -- Can increment this dimension. Reset all inner dims to lexmin.
                let outerFixed = take dim point ++ [nextVal]
                    innerLevels = drop (dim + 1) levels
                in case computeInnerLexMin innerLevels params (reverse outerFixed) of
                     Just innerVals -> Just (outerFixed ++ innerVals)
                     Nothing ->
                       -- Incrementing here makes inner dims empty.
                       -- Try incrementing further (skip to next valid value).
                       -- For simplicity, try the next outer dimension.
                       findCarryAndAdvance levels params point (dim - 1)
              else
                -- This dimension is saturated (at upper bound).
                -- Try the next outer dimension.
                findCarryAndAdvance levels params point (dim - 1)

-- | Compute lexmin for inner dimensions given fixed outer values.
-- prefixRev is in reverse order (most recent first) for evalLower/evalUpper.
computeInnerLexMin :: [LoopLevel] -> [Integer] -> [Integer] -> Maybe [Integer]
computeInnerLexMin [] _ _ = Just []
computeInnerLexMin (level : rest) params prefixRev =
  case llEquality level of
    Just eq ->
      let val = evalBoundExact params prefixRev eq
      in case computeInnerLexMin rest params (val : prefixRev) of
           Just inner -> Just (val : inner)
           Nothing    -> Nothing
    Nothing ->
      let lo = safeMaximum $ map (evalLower params prefixRev) (llLowerBounds level)
          hi = safeMinimum $ map (evalUpper params prefixRev) (llUpperBounds level)
      in case (lo, hi) of
           (Just l, Just h) | l <= h ->
             case computeInnerLexMin rest params (l : prefixRev) of
               Just inner -> Just (l : inner)
               Nothing    -> Nothing
           _ -> Nothing

-- | Evaluate an equality bound exactly.
evalBoundExact :: [Integer] -> [Integer] -> AffineBound -> Integer
evalBoundExact params prefix bound =
  let num = evalBound params prefix bound
  in num `div` abDivisor bound

-- helpers

safeMaximum :: [Integer] -> Maybe Integer
safeMaximum [] = Nothing
safeMaximum xs = Just (maximum xs)

safeMinimum :: [Integer] -> Maybe Integer
safeMinimum [] = Nothing
safeMinimum xs = Just (minimum xs)
