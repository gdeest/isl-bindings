{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Render Alpha 'Expr' trees to C expression strings.
--
-- This module produces the bodies of C statement macros: given an
-- equation body, it emits the C code that computes one point of
-- the recurrence.  ISL generates the surrounding loop structure;
-- this module fills in the innermost computation.
--
-- String building here is for statement macro bodies, not loop
-- generation — loops come from ISL via 'Isl.AstBuild'.
module Alpha.Codegen.ExprRender
  ( RenderCtx(..)
  , renderEquationMacro
  , renderExprToC
  , extractSubscripts
  , extractOneBound
  ) where

import Data.List (intercalate, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, natVal, symbolVal, type (+))

import Isl.Typed.Constraints
  ( Constraint(..), MapIx(..), SetIx(..) )
import qualified Isl.Typed.Constraints as C
import Isl.Typed.Params (KnownSymbols, symbolVals)
import Isl.TypeLevel.Reflection (DomTag, reflectDomConstraints)
import Isl.TypeLevel.Sing (knownConstraints, reifySTConstraintsMapSplit)

import Alpha.Core
import Alpha.Codegen.COp
import Alpha.Allocation (EqStorage(..), Allocation(..))
import qualified Alpha.Polyhedral.Contraction as C


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Render context
-- ═══════════════════════════════════════════════════════════════════════

data RenderCtx = RenderCtx
  { rcParams    :: ![String]              -- parameter names (positional)
  , rcIterVars  :: ![String]              -- current iterator variable names
  , rcStorage   :: !(Map String EqStorage) -- variable → storage strategy
  , rcDomBounds :: !(Map String [String])  -- variable → per-dim exclusive upper bounds
  , rcCType     :: !String                 -- C type name for values (e.g. "double")
  }


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Macro generation
-- ═══════════════════════════════════════════════════════════════════════

-- | Generate a C macro definition for one equation.
--
-- For a non-reduction equation @C[i,j] = body@, emits:
-- @#define C(c0,c1) do { C_buf[...] = body_c; } while(0)@
--
-- For a sum-reduction equation @C[i,j] = sum_k ...@, the macro is
-- the accumulation step: @C_buf[...] += body_c@.
-- The output array must be zero-initialized by the caller.
renderEquationMacro
  :: forall ps decls n (d :: DomTag ps n) a.
     KnownSymbols ps
  => String                -- equation name
  -> Int                   -- number of output dimensions
  -> Alpha.Core.Expr ps decls n d a  -- equation body
  -> RenderCtx
  -> String                -- complete #define line
renderEquationMacro eqName nOutDims body ctx =
  let args    = intercalate "," (rcIterVars ctx)
      -- Write LHS uses only the output dims (first n iter vars)
      writeVars = take nOutDims (rcIterVars ctx)
      writeLhs = renderArrayAccess eqName writeVars ctx
      (accOp, bodyExpr) = case body of
        Reduce ReduceSum _ inner ->
          (" += ", renderExprToC ctx inner)
        Reduce ReduceProd _ inner ->
          (" *= ", renderExprToC ctx inner)
        Reduce ReduceMin _ inner ->
          let bc = renderExprToC ctx inner
          in (" = ", "fmin(" ++ writeLhs ++ ", " ++ bc ++ ")")
        Reduce ReduceMax _ inner ->
          let bc = renderExprToC ctx inner
          in (" = ", "fmax(" ++ writeLhs ++ ", " ++ bc ++ ")")
        _ -> (" = ", renderExprToC ctx body)
  in "#define " ++ eqName ++ "(" ++ args ++ ") do { "
     ++ writeLhs ++ accOp ++ bodyExpr ++ "; } while(0)"


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Expression rendering
-- ═══════════════════════════════════════════════════════════════════════

-- | Render an 'Expr' to a C expression string.
renderExprToC
  :: forall ps decls n (d :: DomTag ps n) a.
     RenderCtx -> Alpha.Core.Expr ps decls n d a -> String

renderExprToC ctx (Var (Proxy :: Proxy name)) =
  let varName = symbolVal (Proxy @name)
  in renderArrayAccess varName (rcIterVars ctx) ctx

renderExprToC _ctx (Const _) = "0.0"

renderExprToC ctx (Pw op e1 e2) =
  renderBinOp op (renderExprToC ctx e1) (renderExprToC ctx e2)

renderExprToC ctx (PMap op e) =
  renderUnaryOp op (renderExprToC ctx e)

renderExprToC ctx
  (Dep (Proxy :: Proxy mapCs) (inner :: Alpha.Core.Expr ps decls no dInner a)) =
  let ni  = length (rcIterVars ctx)
      no_ = fromIntegral (natVal (Proxy @no)) :: Int
      cs  = reifySTConstraintsMapSplit ni
              (knownConstraints @ps @(n + no) @mapCs)
      subs = extractSubscripts ni (rcIterVars ctx) (rcParams ctx) cs
      -- Extract inner domain bounds for the target variable
      innerDomCs = reflectDomConstraints @ps @no @dInner
      innerConjs = [C.Conjunction innerDomCs]
      innerBounds = [ extractOneBound (rcParams ctx) innerConjs d | d <- [0 .. no_ - 1] ]
      innerVarName' = varNameFromExpr inner
      updBounds = case innerVarName' of
        Just vn -> Map.insert vn innerBounds (rcDomBounds ctx)
        Nothing -> rcDomBounds ctx
      innerCtx = ctx { rcIterVars = subs, rcDomBounds = updBounds }
  in renderExprToC innerCtx inner

renderExprToC ctx (Reduce _rop _projCs inner) =
  renderExprToC ctx inner

renderExprToC ctx (Case branches) =
  renderBranches ctx branches

renderBranches
  :: forall ps decls n (amb :: DomTag ps n) branchDoms a.
     RenderCtx -> Branches ps decls n amb branchDoms a -> String
renderBranches _ctx BNil = "0.0 /* unreachable */"
renderBranches ctx (BCons _ body BNil) =
  renderExprToC ctx body
renderBranches ctx (BCons (_ :: Proxy d) body rest) =
  let condCs = reflectDomConstraints @ps @n @d
      condStr = renderDomCondition (rcIterVars ctx) (rcParams ctx) condCs
  in "(" ++ condStr ++ ") ? (" ++ renderExprToC ctx body ++ ") : ("
     ++ renderBranches ctx rest ++ ")"


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Subscript extraction from affine map constraints
-- ═══════════════════════════════════════════════════════════════════════

-- | Extract C subscript expressions from map equality constraints.
--
-- Given equality constraints of the form @OutDim k = f(InDims, Params)@,
-- extract @f@ for each output dim and render to C.
extractSubscripts
  :: Int                     -- number of input dims
  -> [String]                -- input dim variable names
  -> [String]                -- parameter names
  -> [Constraint MapIx]      -- map constraints
  -> [String]                -- one C expression per output dim
extractSubscripts _ni iterVars paramNames cs =
  let pairs = mapMaybe extractEquality cs
      sorted = map snd $ sortBy (comparing fst) pairs
  in sorted
  where
    extractEquality :: Constraint MapIx -> Maybe (Int, String)
    extractEquality (EqualityConstraint expr) =
      case findOutDim expr of
        Just (outIdx, bodyExpr) ->
          Just (outIdx, renderMapExpr iterVars paramNames bodyExpr)
        Nothing -> Nothing
    extractEquality _ = Nothing

    -- | Find the OutDim in an equality @OutDim k + (-1)*f(...) = 0@
    -- and extract @f@.  Standard form from IslMultiAffToMap.
    findOutDim :: C.Expr MapIx -> Maybe (Int, C.Expr MapIx)
    findOutDim (C.Add (C.Ix (OutDim k)) (C.Mul (-1) f)) = Just (k, f)
    findOutDim (C.Add (C.Mul (-1) f) (C.Ix (OutDim k))) = Just (k, f)
    findOutDim (C.Add (C.Ix (OutDim k)) f) = Just (k, negateExpr f)
    findOutDim _ = Nothing

    negateExpr :: C.Expr MapIx -> C.Expr MapIx
    negateExpr (C.Mul k e)    = C.Mul (negate k) e
    negateExpr (C.Constant n) = C.Constant (negate n)
    negateExpr e              = C.Mul (-1) e


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Affine expression → C rendering
-- ═══════════════════════════════════════════════════════════════════════

renderMapExpr :: [String] -> [String] -> C.Expr MapIx -> String
renderMapExpr iterVars paramNames = go
  where
    go (C.Ix (InDim d))
      | d < length iterVars = iterVars !! d
      | otherwise = "d" ++ show d
    go (C.Ix (OutDim k)) = "out" ++ show k
    go (C.Ix (MapParam p))
      | p < length paramNames = paramNames !! p
      | otherwise = "p" ++ show p
    go (C.Constant n) = show n
    go (C.Mul 1 e) = go e
    go (C.Mul (-1) e) = "(-" ++ go e ++ ")"
    go (C.Mul k e) = show k ++ " * " ++ go e
    go (C.Add a b) = "(" ++ go a ++ " + " ++ go b ++ ")"
    go (C.FloorDiv e d) = "floord(" ++ go e ++ ", " ++ show d ++ ")"

renderSetExpr :: [String] -> [String] -> C.Expr SetIx -> String
renderSetExpr iterVars paramNames = go
  where
    go (C.Ix (SetDim d))
      | d < length iterVars = iterVars !! d
      | otherwise = "d" ++ show d
    go (C.Ix (SetParam p))
      | p < length paramNames = paramNames !! p
      | otherwise = "p" ++ show p
    go (C.Constant n) = show n
    go (C.Mul 1 e) = go e
    go (C.Mul (-1) e) = "(-" ++ go e ++ ")"
    go (C.Mul k e) = show k ++ " * " ++ go e
    go (C.Add a b) = "(" ++ go a ++ " + " ++ go b ++ ")"
    go (C.FloorDiv e d) = "floord(" ++ go e ++ ", " ++ show d ++ ")"


-- ═══════════════════════════════════════════════════════════════════════
-- §6. Array access rendering
-- ═══════════════════════════════════════════════════════════════════════

-- | Render an array access with linearized subscripts.
--
-- For a variable with bounds @[N0, N1, ..., N_{n-1}]@, row-major:
-- @buf[d0 * N1 * ... * N_{n-1} + d1 * N2 * ... * N_{n-1} + ... + d_{n-1}]@
renderArrayAccess :: String -> [String] -> RenderCtx -> String
renderArrayAccess varName subs ctx =
  let bufName = varName ++ "_buf"
  in case Map.lookup varName (rcStorage ctx) of
    Just (Contracted stor) ->
      let physSubs = map (renderStorageExpr subs (rcParams ctx)) (C.smExprs stor)
      in bufName ++ "[" ++ linearize varName physSubs ctx ++ "]"
    _ ->
      bufName ++ "[" ++ linearize varName subs ctx ++ "]"

-- | Render a storage map expression with concrete subscript values.
renderStorageExpr :: [String] -> [String] -> C.Expr SetIx -> String
renderStorageExpr subs paramNames = go
  where
    go (C.Ix (SetDim d))
      | d < length subs = subs !! d
      | otherwise = "d" ++ show d
    go (C.Ix (SetParam p))
      | p < length paramNames = paramNames !! p
      | otherwise = "p" ++ show p
    go (C.Constant n) = show n
    go (C.Mul 1 e) = go e
    go (C.Mul k e) = show k ++ " * " ++ go e
    go (C.Add a b) = "(" ++ go a ++ " + " ++ go b ++ ")"
    go (C.FloorDiv e d) = "floord(" ++ go e ++ ", " ++ show d ++ ")"

-- | Linearize subscripts with row-major strides.
linearize :: String -> [String] -> RenderCtx -> String
linearize varName subs ctx = case subs of
  []  -> "0"
  [s] -> s
  _   -> case Map.lookup varName (rcDomBounds ctx) of
    Just bounds -> linearizeWithBounds subs bounds
    Nothing     -> intercalate " + " subs  -- fallback: no stride info

linearizeWithBounds :: [String] -> [String] -> String
linearizeWithBounds subs bounds =
  let n = length subs
      strides = [ concatMap (\j -> bounds !! j ++ " * ") [i+1..n-1]
                | i <- [0..n-1] ]
      terms = zipWith (\sub stride ->
        if null stride then sub
        else stride ++ sub) subs strides
  in intercalate " + " terms


-- ═══════════════════════════════════════════════════════════════════════
-- §7. Domain bound extraction (pure, constraint-based)
-- ═══════════════════════════════════════════════════════════════════════

-- | Extract the exclusive upper bound for one dimension from constraints.
extractOneBound :: [String] -> [C.Conjunction SetIx] -> Int -> String
extractOneBound params conjs dim =
  let ineqs = [ e | C.Conjunction cs <- conjs, InequalityConstraint e <- cs ]
  in case mapMaybe (matchUB dim params) ineqs of
    (ub:_) -> ub
    []     -> "/* unknown */"

matchUB :: Int -> [String] -> C.Expr SetIx -> Maybe String
matchUB d ps expr = matchUBN d ps (simplifyConst expr)

simplifyConst :: C.Expr ix -> C.Expr ix
simplifyConst (C.Mul k (C.Constant n)) = C.Constant (k * n)
simplifyConst (C.Add a b) = C.Add (simplifyConst a) (simplifyConst b)
simplifyConst (C.Mul k e) = C.Mul k (simplifyConst e)
simplifyConst e = e

matchUBN :: Int -> [String] -> C.Expr SetIx -> Maybe String
matchUBN d ps (C.Add (C.Add (C.Ix (SetParam p)) (C.Constant (-1))) (C.Mul (-1) (C.Ix (SetDim d'))))
  | d == d', p < length ps = Just (ps !! p)
matchUBN d ps (C.Add (C.Ix (SetParam p)) (C.Add (C.Constant (-1)) (C.Mul (-1) (C.Ix (SetDim d')))))
  | d == d', p < length ps = Just (ps !! p)
matchUBN d ps (C.Add (C.Ix (SetParam p)) (C.Mul (-1) (C.Ix (SetDim d'))))
  | d == d', p < length ps = Just (ps !! p ++ " + 1")
matchUBN d _ps (C.Add (C.Constant k) (C.Mul (-1) (C.Ix (SetDim d'))))
  | d == d' = Just (show (k + 1))
matchUBN _ _ _ = Nothing

-- | Extract the variable name from the innermost Var in an Expr.
varNameFromExpr :: forall ps decls n d a. Alpha.Core.Expr ps decls n d a -> Maybe String
varNameFromExpr (Var (Proxy :: Proxy name)) = Just (symbolVal (Proxy @name))
varNameFromExpr (PMap _ e) = varNameFromExpr e
varNameFromExpr (Dep _ inner) = varNameFromExpr inner
varNameFromExpr _ = Nothing


-- ═══════════════════════════════════════════════════════════════════════
-- §8. Binary/unary op rendering
-- ═══════════════════════════════════════════════════════════════════════

renderBinOp :: BinOp -> String -> String -> String
renderBinOp OpAdd a b = "(" ++ a ++ " + " ++ b ++ ")"
renderBinOp OpSub a b = "(" ++ a ++ " - " ++ b ++ ")"
renderBinOp OpMul a b = "(" ++ a ++ " * " ++ b ++ ")"
renderBinOp OpDiv a b = "(" ++ a ++ " / " ++ b ++ ")"
renderBinOp OpMin a b = "fmin(" ++ a ++ ", " ++ b ++ ")"
renderBinOp OpMax a b = "fmax(" ++ a ++ ", " ++ b ++ ")"

renderUnaryOp :: UnaryOp -> String -> String
renderUnaryOp OpNeg   s = "(-(" ++ s ++ "))"
renderUnaryOp OpAbs   s = "fabs(" ++ s ++ ")"
renderUnaryOp OpFloor s = "floor(" ++ s ++ ")"
renderUnaryOp OpCeil  s = "ceil(" ++ s ++ ")"
renderUnaryOp OpSqrt  s = "sqrt(" ++ s ++ ")"


-- ═══════════════════════════════════════════════════════════════════════
-- §8. Domain condition rendering (for Case branches)
-- ═══════════════════════════════════════════════════════════════════════

-- | Render domain constraints as a C boolean expression.
renderDomCondition :: [String] -> [String] -> [Constraint SetIx] -> String
renderDomCondition iterVars paramNames cs =
  case mapMaybe renderOneConstraint cs of
    []    -> "1"
    [c]   -> c
    parts -> intercalate " && " parts
  where
    renderOneConstraint (EqualityConstraint e) =
      Just (renderSetExpr iterVars paramNames e ++ " == 0")
    renderOneConstraint (InequalityConstraint e) =
      Just (renderSetExpr iterVars paramNames e ++ " >= 0")
