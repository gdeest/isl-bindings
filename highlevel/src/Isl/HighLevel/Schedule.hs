{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Schedule definition as typed quasi-affine expressions.
--
-- A schedule maps each iteration point to a time-space coordinate vector.
-- Expressed as a list of output expressions over input dimensions —
-- fully typed, supporting floor division for tiling.
--
-- @
-- -- Tiled+skewed Jacobi schedule:
-- sched = tile 0 4 . skew 1 1 0 . skew 2 1 0 $ identity 3
-- -- Result: [floor(t/4), floor((t+i)/32), floor((t+j)/32), t, t+i, t+j]
-- @
module Isl.HighLevel.Schedule
  ( -- * Schedule type
    ScheduleDef(..)
    -- * Constructors
  , identity
    -- * Transformations
  , skew
  , tile
  , interchange
  , shift
    -- * Conversion to NamedMap
  , schedToNamedMap
  ) where

import GHC.TypeLits (Symbol)

import Isl.HighLevel.Constraints (Expr(..), MapIx(..), Constraint(..), Conjunction(..))
import Isl.HighLevel.Pure (NamedMap(..))
import Isl.HighLevel.Params (KnownSymbols(symbolVals))
import Isl.HighLevel.Params (KnownSymbols)


-- | A schedule is a list of quasi-affine output expressions over input dimensions.
-- Each expression is an @Expr MapIx@ where @InDim k@ references input dim k.
newtype ScheduleDef = ScheduleDef { schedExprs :: [Expr MapIx] }
  deriving (Show)


-- | Identity schedule for @n@ dimensions: @[d0, d1, .., d_{n-1}]@.
identity :: Int -> ScheduleDef
identity n = ScheduleDef [Ix (InDim k) | k <- [0..n-1]]


-- | Skew: replace output dim @targetDim@ with @sched[targetDim] + factor * sched[refDim]@.
--
-- @skew 1 1 0@ on @[t, i, j]@ → @[t, i + t, j]@
skew :: Int       -- ^ target dimension index (in current schedule)
     -> Integer   -- ^ skew factor
     -> Int       -- ^ reference dimension index (in current schedule)
     -> ScheduleDef -> ScheduleDef
skew target factor ref (ScheduleDef es) =
  let refExpr = es !! ref
      targetExpr = es !! target
      skewed = Add targetExpr (Mul factor refExpr)
  in ScheduleDef (replaceAt target skewed es)


-- | Tile: insert @floor(sched[dim] / tileSize)@ before @dim@.
-- Adds one output dimension.
--
-- @tile 0 4@ on @[t, i, j]@ → @[floor(t/4), t, i, j]@
tile :: Int       -- ^ dimension index to tile
     -> Integer   -- ^ tile size
     -> ScheduleDef -> ScheduleDef
tile dim tileSize (ScheduleDef es) =
  let tileExpr = FloorDiv (es !! dim) tileSize
  in ScheduleDef (insertAt dim tileExpr es)


-- | Interchange: swap two output dimensions.
interchange :: Int -> Int -> ScheduleDef -> ScheduleDef
interchange a b (ScheduleDef es) =
  let ea = es !! a; eb = es !! b
  in ScheduleDef (replaceAt a eb (replaceAt b ea es))


-- | Shift: add a constant offset to an output dimension.
shift :: Int -> Integer -> ScheduleDef -> ScheduleDef
shift dim offset (ScheduleDef es) =
  let shifted = Add (es !! dim) (Constant offset)
  in ScheduleDef (replaceAt dim shifted es)


-- | Convert a schedule to a 'NamedMap' for ISL operations.
--
-- The resulting map is: @{ stmtName[in_dims] → [sched_exprs] : domain }@
-- where domain constraints are @1 <= d_k <= param_k@.
schedToNamedMap :: forall ps. KnownSymbols ps
  => String        -- ^ statement name
  -> Int           -- ^ number of input dimensions
  -> ScheduleDef   -- ^ schedule
  -> NamedMap
schedToNamedMap stmtName nIn (ScheduleDef outExprs) =
  let nOut = length outExprs
      params = symbolVals @ps
      nParams = length params
      -- Output constraints: out_k = expr_k(in_dims)
      -- Express as: out_k - expr_k = 0 (equality)
      schedConstrs =
        [ EqualityConstraint (Add (Ix (OutDim k)) (negateExpr (liftExpr (outExprs !! k))))
        | k <- [0..nOut-1]
        ]
      -- Domain: 1 <= in_k <= param_k
      domConstrs = concat
        [ [ InequalityConstraint (Add (Ix (InDim k)) (Constant (-1)))
          , InequalityConstraint (Add (Ix (MapParam k)) (Mul (-1) (Ix (InDim k))))
          ]
        | k <- [0..nIn-1], k < nParams
        ]
  in NamedMap
    { nmDomainName = Just stmtName
    , nmRangeName  = Nothing  -- schedule range is unnamed time-space
    , nmParams     = params
    , nmNIn        = nIn
    , nmNOut       = nOut
    , nmConjs      = [Conjunction (schedConstrs ++ domConstrs)]
    }

-- | Lift an expr that references InDim only (from ScheduleDef context)
-- into the combined (InDim + OutDim) MapIx space. InDim stays InDim.
liftExpr :: Expr MapIx -> Expr MapIx
liftExpr = id  -- InDim/MapParam are already the right constructors

-- | Negate an expression: @negate e = -1 * e@.
negateExpr :: Expr MapIx -> Expr MapIx
negateExpr e = Mul (-1) e


-- Helpers

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 x (_:ys) = x : ys
replaceAt n x (y:ys) = y : replaceAt (n-1) x ys

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x ys = x : ys
insertAt n x (y:ys) = y : insertAt (n-1) x ys
insertAt _ x [] = [x]
