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
module Alpha.Polyhedral.Schedule
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
  , schedToNamedMap'
  ) where

import GHC.TypeLits (Symbol)

import Isl.Typed.Constraints (Expr(..), SetIx(..), MapIx(..), Constraint(..), Conjunction(..))
import Isl.Typed.Constraints (NamedMap(..), NamedSet(..))
import Isl.Typed.Params (KnownSymbols(symbolVals))
import Isl.Typed.Params (KnownSymbols)


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
-- Domain is a 'NamedSet' — its shape drives the input dimensions.
-- The resulting map is: @{ stmtName[in_dims] → [sched_exprs] : user_domain }@
schedToNamedMap :: forall ps. KnownSymbols ps
  => String              -- ^ statement name
  -> NamedSet            -- ^ iteration domain (provides constraints + nDims)
  -> ScheduleDef         -- ^ schedule
  -> NamedMap
schedToNamedMap stmtName dom (ScheduleDef outExprs) =
  let domConstrs = case nsConjs dom of
        (Conjunction cs : _) -> cs
        []                   -> []
      nIn = nsNDims dom
      nOut = length outExprs
      params = symbolVals @ps
      -- Output constraints: out_k = expr_k(in_dims)
      schedConstrs =
        [ EqualityConstraint (Add (Ix (OutDim k)) (negateExpr (outExprs !! k)))
        | k <- [0..nOut-1]
        ]
      -- Domain constraints converted from SetIx to MapIx
      mapDomConstrs = map (mapConstraint setToMapIx) domConstrs
  in NamedMap
    { nmDomainName = Just stmtName
    , nmRangeName  = Nothing
    , nmParams     = params
    , nmNIn        = nIn
    , nmNOut       = nOut
    , nmConjs      = [Conjunction (schedConstrs ++ mapDomConstrs)]
    }

-- | Like 'schedToNamedMap' but takes params from the domain instead of
-- the type-level list.  Used when the domain has extra parameters (e.g., TJ)
-- that aren't in the spec's type-level parameter list.
schedToNamedMap'
  :: String              -- ^ statement name
  -> NamedSet            -- ^ iteration domain (provides params, constraints, nDims)
  -> ScheduleDef         -- ^ schedule
  -> NamedMap
schedToNamedMap' stmtName dom (ScheduleDef outExprs) =
  let domConstrs = concatMap (\(Conjunction cs) -> cs) (nsConjs dom)
      nIn = nsNDims dom
      nOut = length outExprs
      params = nsParams dom
      schedConstrs =
        [ EqualityConstraint (Add (Ix (OutDim k)) (negateExpr (outExprs !! k)))
        | k <- [0..nOut-1]
        ]
      mapDomConstrs = map (mapConstraint setToMapIx) domConstrs
  in NamedMap
    { nmDomainName = Just stmtName
    , nmRangeName  = Nothing
    , nmParams     = params
    , nmNIn        = nIn
    , nmNOut       = nOut
    , nmConjs      = [Conjunction (schedConstrs ++ mapDomConstrs)]
    }

-- | Convert SetIx to MapIx.
setToMapIx :: SetIx -> MapIx
setToMapIx (SetDim d)   = InDim d
setToMapIx (SetParam p) = MapParam p

mapConstraint :: (a -> b) -> Constraint a -> Constraint b
mapConstraint f (EqualityConstraint e)   = EqualityConstraint (fmap f e)
mapConstraint f (InequalityConstraint e) = InequalityConstraint (fmap f e)

-- | Extract set dimension indices from a constraint.
setDimsIn :: Constraint SetIx -> [Int]
setDimsIn (EqualityConstraint e)   = exprSetDims e
setDimsIn (InequalityConstraint e) = exprSetDims e

exprSetDims :: Expr SetIx -> [Int]
exprSetDims (Ix (SetDim d))  = [d]
exprSetDims (Ix _)           = []
exprSetDims (Constant _)     = []
exprSetDims (Add a b)        = exprSetDims a ++ exprSetDims b
exprSetDims (Mul _ e)        = exprSetDims e
exprSetDims (FloorDiv e _)   = exprSetDims e

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
