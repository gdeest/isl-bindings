-- | Core data types for the Boulet-Feautrier polyhedra scanner.
--
-- These types represent a "compiled" form of a polyhedron — a nested
-- loop descriptor that can enumerate all integer points without
-- referencing ISL at runtime.
module Isl.Scan.Types
  ( AffineBound(..)
  , LoopLevel(..)
  , LoopNest(..)
  , Scanner(..)
  ) where

-- | An affine function of outer loop variables and parameters.
--
-- Represents: @(sum of loopCoeffs + sum of paramCoeffs + constant) / divisor@
--
-- For lower bounds, use @ceilDiv@; for upper bounds, use @floorDiv@.
data AffineBound = AffineBound
  { abLoopCoeffs  :: [(Integer, Int)]
    -- ^ @(coefficient, loop variable index)@ — the loop variable is
    -- an outer dimension that has already been assigned a value.
  , abParamCoeffs :: [(Integer, Int)]
    -- ^ @(coefficient, parameter index)@
  , abConstant    :: Integer
  , abDivisor     :: Integer
    -- ^ Always positive. For bounds with coefficient > 1 on the
    -- iterating variable, the divisor captures the integer division.
  } deriving (Show, Eq)

-- | A single loop level in a nested loop.
data LoopLevel = LoopLevel
  { llDim         :: Int
    -- ^ Which original set dimension this level iterates over.
  , llLowerBounds :: [AffineBound]
    -- ^ @x_k >= max(ceil(lower_i))@ for each lower bound.
  , llUpperBounds :: [AffineBound]
    -- ^ @x_k <= min(floor(upper_j))@ for each upper bound.
  , llEquality    :: Maybe AffineBound
    -- ^ If the dimension is equality-constrained: @x_k = expr@.
    -- When present, lower/upper bounds are ignored.
  , llStride      :: Integer
    -- ^ Loop step (usually 1).
  } deriving (Show, Eq)

-- | A complete loop nest for one convex polyhedron (basic set).
-- Levels are ordered outermost to innermost.
data LoopNest = LoopNest
  { lnLevels :: [LoopLevel]
  , lnParams :: Int
  , lnDims   :: Int
  } deriving (Show, Eq)

-- | A scanner for a (possibly non-convex) set: union of loop nests.
-- Each loop nest corresponds to one basic set in the disjoint decomposition.
newtype Scanner = Scanner [LoopNest]
  deriving (Show, Eq)
