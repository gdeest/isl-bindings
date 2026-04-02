-- | Render 'Expr' (from "Isl.HighLevel.Constraints") to C code strings.
--
-- Handles all expression constructors including 'FloorDiv'.
-- For set expressions, dims and params are resolved via name lists.
-- For map expressions, input/output dims are resolved separately.
module Isl.Infer.Codegen.Expr
  ( setExprToC
  , mapExprToC
  ) where

import Isl.HighLevel.Constraints (Expr(..), SetIx(..), MapIx(..))

-- | Render a set expression to C.
-- @dimNames@ maps 'SetDim' indices, @paramNames@ maps 'SetParam' indices.
setExprToC :: [String] -> [String] -> Expr SetIx -> String
setExprToC dimNames paramNames = go
  where
    go (Ix (SetDim d))   = dimNames !! d
    go (Ix (SetParam p)) = paramNames !! p
    go (Constant n)      = show n
    go (Add a b)         = "(" ++ go a ++ " + " ++ go b ++ ")"
    go (Mul k e)         = "(" ++ show k ++ " * " ++ go e ++ ")"
    go (FloorDiv e d)    = "ISL_FLOOR_DIV((" ++ go e ++ "), " ++ show d ++ ")"

-- | Render a map expression to C.
-- @inNames@ maps 'InDim', @outNames@ maps 'OutDim', @paramNames@ maps 'MapParam'.
mapExprToC :: [String] -> [String] -> [String] -> Expr MapIx -> String
mapExprToC inNames outNames paramNames = go
  where
    go (Ix (InDim d))    = inNames !! d
    go (Ix (OutDim d))   = outNames !! d
    go (Ix (MapParam p)) = paramNames !! p
    go (Constant n)      = show n
    go (Add a b)         = "(" ++ go a ++ " + " ++ go b ++ ")"
    go (Mul k e)         = "(" ++ show k ++ " * " ++ go e ++ ")"
    go (FloorDiv e d)    = "ISL_FLOOR_DIV((" ++ go e ++ "), " ++ show d ++ ")"
