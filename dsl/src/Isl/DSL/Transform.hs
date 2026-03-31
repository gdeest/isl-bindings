-- | Schedule transform application.
--
-- Transforms modify the schedule expressions that map iteration coords
-- to time-space coords. Each transform is applied to the interleaved
-- schedule @[seq0, d0, seq1, d1, ...]@ and produces a new schedule.
module Isl.DSL.Transform
  ( applyTransforms
  , applyTransform
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Isl.DSL.Types


-- | Apply a list of transforms to the interleaved schedule of a statement.
-- The schedule is a list of @(dimName, AExpr)@ pairs (including seq dims).
-- Returns the transformed schedule plus a set of parallel dims.
applyTransforms :: [Transform]
                -> [String]                -- ^ iteration dim names
                -> [(String, AExpr)]       -- ^ initial interleaved schedule: [(name, expr)]
                -> ([(String, AExpr)], [String])  -- ^ (transformed sched, parallel dim names)
applyTransforms xforms dimNames = go xforms []
  where
    go [] parDims sched = (sched, parDims)
    go (t:ts) parDims sched = case applyTransform t dimNames sched of
      (sched', newPars) -> go ts (parDims ++ newPars) sched'


-- | Apply a single transform. Returns (new schedule, newly parallel dim names).
applyTransform :: Transform
               -> [String]              -- ^ iteration dim names
               -> [(String, AExpr)]     -- ^ current schedule
               -> ([(String, AExpr)], [String])
applyTransform (TSkew d0 d1 factor) dimNames sched =
  -- Skew: replace the expression for d0 with d0_expr + factor * d1_expr
  -- Find d0 and d1 in the schedule (by matching dim names)
  let d0Expr = lookupDimExpr d0 sched
      d1Expr = lookupDimExpr d1 sched
      skewedExpr = AAdd d0Expr (AMul factor d1Expr)
      sched' = replaceDimExpr d0 skewedExpr sched
  in (sched', [])

applyTransform (TTile dimName tileSize) _dimNames sched =
  -- Tile: split dim into tile_dim (floor(d/T)) and point_dim (d)
  -- Insert tile dim before the original dim
  let dExpr = lookupDimExpr dimName sched
      tileName = dimName ++ "_tile"
      tileExpr = AFloorDiv dExpr tileSize
      sched' = insertBefore dimName (tileName, tileExpr) sched
  in (sched', [])

applyTransform (TInterchange d0 d1) _dimNames sched =
  -- Swap two dims in the schedule
  let sched' = swapDims d0 d1 sched
  in (sched', [])

applyTransform (TParallel dimName) _dimNames sched =
  (sched, [dimName])

applyTransform (TShift dimName offset) _dimNames sched =
  let dExpr = lookupDimExpr dimName sched
      sched' = replaceDimExpr dimName (AAdd dExpr (ALit offset)) sched
  in (sched', [])

applyTransform (TUnroll _dimName _factor) _dimNames sched =
  -- Unrolling doesn't change the schedule, it's a codegen hint
  (sched, [])

applyTransform (TFuse _d0 _d1) _dimNames sched =
  -- Fusion merges sibling bands — requires tree-level manipulation
  -- For now, no-op (TODO)
  (sched, [])

applyTransform (TFiss _dimName _at) _dimNames sched =
  (sched, [])

applyTransform (TMapBand bandName f) _dimNames sched =
  -- Generic: apply arbitrary quasi-affine map to the named band
  let ctx = schedToCtx sched
      newDims = f ctx
      -- Replace all dims in the schedule with the new ones
      -- (drops existing dims, replaces with f's output)
      seqDims = [(n, e) | (n, e) <- sched, isSeqDim n]
      -- Keep seq dims, replace content dims with mapBand output
      sched' = replaceContentDims sched newDims
  in (sched', [])


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Build a SchedCtx from the current schedule.
schedToCtx :: [(String, AExpr)] -> SchedCtx
schedToCtx = SchedCtx . Map.fromList

-- | Look up a dimension's expression by name.
lookupDimExpr :: String -> [(String, AExpr)] -> AExpr
lookupDimExpr name sched =
  case lookup name sched of
    Just e  -> e
    Nothing -> error $ "lookupDimExpr: dim " ++ show name ++ " not found in schedule "
                    ++ show (map fst sched)

-- | Replace a dimension's expression.
replaceDimExpr :: String -> AExpr -> [(String, AExpr)] -> [(String, AExpr)]
replaceDimExpr name newExpr = map $ \(n, e) ->
  if n == name then (n, newExpr) else (n, e)

-- | Insert a new dim before an existing one.
insertBefore :: String -> (String, AExpr) -> [(String, AExpr)] -> [(String, AExpr)]
insertBefore _ _ [] = []
insertBefore target new (d@(n, _):ds)
  | n == target = new : d : ds
  | otherwise   = d : insertBefore target new ds

-- | Swap two dims in the schedule.
swapDims :: String -> String -> [(String, AExpr)] -> [(String, AExpr)]
swapDims n0 n1 sched =
  let e0 = lookupDimExpr n0 sched
      e1 = lookupDimExpr n1 sched
  in map (\(n, e) ->
    if n == n0 then (n0, e1)
    else if n == n1 then (n1, e0)
    else (n, e)) sched

-- | Is this a sequential-index dim? (name starts with "_seq")
isSeqDim :: String -> Bool
isSeqDim ('_':'s':'e':'q':_) = True
isSeqDim _ = False

-- | Replace content (non-seq) dims with new dims from mapBand.
replaceContentDims :: [(String, AExpr)] -> [(String, AExpr)] -> [(String, AExpr)]
replaceContentDims oldSched newDims = go oldSched newDims
  where
    go [] _ = []
    go ((n, e):rest) news
      | isSeqDim n = (n, e) : go rest news  -- keep seq dims
      | otherwise = case news of
          (nn, ne):ns -> (nn, ne) : go rest ns
          []          -> go rest []  -- fewer new dims than old
