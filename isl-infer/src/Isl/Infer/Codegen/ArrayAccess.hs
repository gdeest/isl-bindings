-- | Declarative array access code generation.
--
-- Given a 'StorageMap' and stencil offsets, generates C expressions for
-- array reads and writes. Handles modular contraction, row-major
-- linearization, and multi-dimensional buffer indexing.
--
-- @
-- storage = modularTime 3 5  -- A[t,i,j] → buf[t%5, i, j]
-- access  = mkAccess storage ["_t_","_i_","_j_"] "buf" "stride" [0,0,0]
-- -- Result: "buf[(((_t_) % 5) + 5) % 5][(_i_) * stride + (_j_)]"
--
-- body = mkStencilBody storage ["_t_","_i_","_j_"] "buf" "stride"
--          [[-1,-1,0],[-1,1,0],[-1,0,-1],[-1,0,1]]
--          (\\reads -> "(" ++ intercalate " + " reads ++ ") * 0.25")
-- @
module Isl.Infer.Codegen.ArrayAccess
  ( -- * Single array access
    mkAccess
    -- * Full stencil body
  , mkStencilBody
    -- * Linearization helpers
  , linearizeRowMajor
  , renderStorageExpr
  ) where

import Data.List (intercalate)
import Isl.HighLevel.Constraints (Expr(..), SetIx(..), modExpr)
import Isl.HighLevel.Contraction (StorageMap(..))
import Isl.Infer.Codegen.Expr (setExprToC)


-- | Generate a C expression for one array access through a storage map.
--
-- The storage map's expressions are evaluated with the given offsets
-- applied to the iteration coordinates. The first physical dimension
-- indexes the buffer plane; remaining dimensions are linearized row-major.
--
-- @mkAccess storage dimNames bufName strideName offsets@
mkAccess :: StorageMap   -- ^ physical storage mapping
         -> [String]     -- ^ iteration dimension names (macro params)
         -> String       -- ^ buffer array name (e.g., "buf")
         -> String       -- ^ stride variable name (e.g., "stride")
         -> [Int]        -- ^ offsets to apply (one per iteration dim)
         -> String       -- ^ C expression for the array element
mkAccess storage dimNames bufName strideName offsets =
  let physExprs = applyOffsets (smExprs storage) offsets
      rendered  = map (renderStorageExpr dimNames) physExprs
  in case rendered of
    []     -> bufName ++ "[0]"
    [p]    -> bufName ++ "[" ++ p ++ "]"
    (p:ps) -> bufName ++ "[" ++ p ++ "]"
           ++ "[" ++ linearizeRowMajor strideName ps ++ "]"


-- | Generate the full C macro body for a stencil statement.
--
-- Produces: @writeAccess = combination(readAccess_1, ..., readAccess_n);@
mkStencilBody :: StorageMap   -- ^ physical storage mapping
              -> [String]     -- ^ iteration dimension names (macro params)
              -> String       -- ^ buffer array name
              -> String       -- ^ stride variable name
              -> [[Int]]      -- ^ read offset vectors (from StencilDef)
              -> ([String] -> String)  -- ^ combination function on rendered read exprs
              -> String       -- ^ C statement (assignment)
mkStencilBody storage dimNames bufName strideName readOffsets combine =
  let writeExpr = mkAccess storage dimNames bufName strideName (replicate (length dimNames) 0)
      readExprs = [ mkAccess storage dimNames bufName strideName off | off <- readOffsets ]
  in writeExpr ++ " = " ++ combine readExprs ++ ";"


-- | Linearize dimensions row-major with a stride variable.
--
-- @linearizeRowMajor "stride" ["i", "j"]@ = @"(i) * stride + (j)"@
-- @linearizeRowMajor "stride" ["i", "j", "k"]@ = @"((i) * stride + (j)) * stride + (k)"@
-- @linearizeRowMajor "stride" ["i"]@ = @"(i)"@
linearizeRowMajor :: String -> [String] -> String
linearizeRowMajor _      []     = "0"
linearizeRowMajor _      [x]    = "(" ++ x ++ ")"
linearizeRowMajor stride (x:xs) =
  foldl (\acc e -> "(" ++ acc ++ ") * " ++ stride ++ " + (" ++ e ++ ")")
        ("(" ++ x ++ ")")
        xs


-- | Render a storage expression to C, with proper parenthesization.
-- Uses 'setExprToC' with dim names as the variable mapping.
-- Wraps the result in a safe modular form for negative values:
-- @((expr % K) + K) % K@ when the expression contains modular arithmetic.
renderStorageExpr :: [String] -> Expr SetIx -> String
renderStorageExpr dimNames expr = setExprToC dimNames [] expr


-- | Apply integer offsets to storage map expressions.
-- Substitutes @SetDim k@ with @SetDim k + offset_k@ in each expression.
applyOffsets :: [Expr SetIx] -> [Int] -> [Expr SetIx]
applyOffsets exprs offsets = map (substituteOffsets offsets) exprs

-- | Substitute offsets into an expression: SetDim k → SetDim k + offset_k.
substituteOffsets :: [Int] -> Expr SetIx -> Expr SetIx
substituteOffsets offsets = go
  where
    go (Ix (SetDim k))
      | k < length offsets && offsets !! k /= 0
                    = Add (Ix (SetDim k)) (Constant (fromIntegral (offsets !! k)))
      | otherwise   = Ix (SetDim k)
    go (Ix ix)      = Ix ix
    go (Constant n) = Constant n
    go (Add a b)    = Add (go a) (go b)
    go (Mul k e)    = Mul k (go e)
    go (FloorDiv e d) = FloorDiv (go e) d
