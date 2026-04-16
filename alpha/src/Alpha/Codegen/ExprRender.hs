{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Render Alpha 'Expr' trees to C statement macro bodies.
module Alpha.Codegen.ExprRender
  ( RenderCtx(..)
  , renderEquationMacro
  , renderExprToC
  , extractSubscripts
  , extractOneBound
  , extractBoundsISLM
  , BoundErr(..)
  , descCType
  , descMathSuffix
  ) where

import Control.DeepSeq (NFData(..))
import Data.List (intercalate, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (symbolVal, type (+))

import Isl.Typed.Constraints
  ( Constraint(..), MapIx(..), SetIx(..) )
import qualified Isl.Typed.Constraints as C
import Isl.Typed.Params (KnownSymbols)
import Isl.TypeLevel.Reflection (DomTag, reflectDomConstraints)
import Isl.TypeLevel.Sing (knownConstraints, reifySTConstraintsMapSplit)
import Isl.Monad (IslT, Ur(..))
import Isl.Linear (query, query_, freeM, Both(..))
import qualified Isl.Linear as Isl
import qualified Isl.Set as RS
import qualified Isl.PwAff as PA
import qualified Isl.Val as Val

import Alpha.Core
import Alpha.Codegen.COp
import Alpha.Allocation (EqStorage(..))
import Alpha.Scalar
  ( ScalarDesc(..), cTypeName, cMathSuffix
  , ConstBridge(..), AlphaScalar(..) )
import qualified Alpha.Polyhedral.Contraction as C


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Render context
-- ═══════════════════════════════════════════════════════════════════════

data RenderCtx = RenderCtx
  { rcParams    :: ![String]              -- parameter names (positional)
  , rcIterVars  :: ![String]              -- current iterator variable names
  , rcStorage   :: !(Map String EqStorage) -- variable → storage strategy
  , rcDomBounds :: !(Map String [String])  -- variable → per-dim exclusive upper bounds
  , rcDesc      :: !ScalarDesc             -- scalar type descriptor for this equation
  }

descCType :: ScalarDesc -> String
descCType (MkScalarDesc { sdCNumType = ct }) = cTypeName ct

descMathSuffix :: ScalarDesc -> String
descMathSuffix (MkScalarDesc { sdCNumType = ct }) = cMathSuffix ct


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
        Reduce rop _ inner ->
          reduceOpToC rop (rcDesc ctx) writeLhs (renderExprToC ctx inner)
        _ -> (" = ", renderExprToC ctx body)
  in "#define " ++ eqName ++ "(" ++ args ++ ") do { "
     ++ writeLhs ++ accOp ++ bodyExpr ++ "; } while(0)"

-- | Lower a 'ReduceOp' to the pair @(accOp, bodyExpr)@ used to build
-- the equation's statement macro.  Exhaustive on 'ReduceOp' by design;
-- see the module pragma.  Kept un-exported and local to this module.
reduceOpToC :: ReduceOp -> ScalarDesc -> String -> String -> (String, String)
reduceOpToC ReduceSum  _    _    bc = (" += ", bc)
reduceOpToC ReduceProd _    _    bc = (" *= ", bc)
reduceOpToC ReduceMin  desc lhs  bc =
  let sfx = descMathSuffix desc
  in  (" = ", "fmin" ++ sfx ++ "(" ++ lhs ++ ", " ++ bc ++ ")")
reduceOpToC ReduceMax  desc lhs  bc =
  let sfx = descMathSuffix desc
  in  (" = ", "fmax" ++ sfx ++ "(" ++ lhs ++ ", " ++ bc ++ ")")


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

renderExprToC _ctx (Const v) =
  -- The 'AlphaScalar' dict carried by 'Const' gives us the bridge
  -- directly — no 'rcDesc'-keyed lookup, no type-unsafe cast (#1).
  showLiteral scalarConstBridge v

renderExprToC ctx (Pw op e1 e2) =
  let sfx = descMathSuffix (rcDesc ctx)
  in renderBinOp sfx op (renderExprToC ctx e1) (renderExprToC ctx e2)

renderExprToC ctx (PMap op e) =
  let sfx = descMathSuffix (rcDesc ctx)
  in renderUnaryOp sfx op (renderExprToC ctx e)

renderExprToC ctx
  (Dep (Proxy :: Proxy mapCs) (inner :: Alpha.Core.Expr ps decls no dInner a)) =
  let ni  = length (rcIterVars ctx)
      cs  = reifySTConstraintsMapSplit ni
              (knownConstraints @ps @(n + no) @mapCs)
      subs = extractSubscripts ni (rcIterVars ctx) (rcParams ctx) cs
      -- Inner variable bounds are precomputed at the top of codegen
      -- from the declared-variable domains (inputs/outputs/locals).
      -- No ISL calls happen here — the renderer is a pure function.
      innerCtx = ctx { rcIterVars = subs }
  in renderExprToC innerCtx inner

renderExprToC ctx (Reduce _rop _projCs inner) =
  renderExprToC ctx inner

renderExprToC ctx (Case branches) =
  renderBranches ctx branches

renderBranches
  :: forall ps decls n (amb :: DomTag ps n) branchDoms a.
     RenderCtx -> Branches ps decls n amb branchDoms a -> String
renderBranches _ctx BNil = "0 /* unreachable */"
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

renderArrayAccess :: String -> [String] -> RenderCtx -> String
renderArrayAccess varName subs ctx =
  let bufName = varName ++ "_buf"
  in case Map.lookup varName (rcStorage ctx) of
    Just (Contracted stor) ->
      let physSubs = map (renderStorageExpr subs (rcParams ctx)) (C.smExprs stor)
      in bufName ++ "[" ++ linearize varName physSubs ctx ++ "]"
    _ ->
      bufName ++ "[" ++ linearize varName subs ctx ++ "]"

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
--
-- Precondition: @rcDomBounds@ contains an entry for every declared
-- variable (enforced at the codegen entry point by 'declListBoundsM').
-- A missing entry for a multi-dim access indicates a codegen bug —
-- fail loudly rather than emit a silently-wrong @sub0 + sub1 + ...@.
linearize :: String -> [String] -> RenderCtx -> String
linearize varName subs ctx = case subs of
  []  -> "0"
  [s] -> s
  _   -> case Map.lookup varName (rcDomBounds ctx) of
    Just bounds -> linearizeWithBounds subs bounds
    Nothing -> error $
      "Alpha.Codegen.ExprRender.linearize: no bounds for \""
      ++ varName ++ "\" (multi-dim access requires strides — \
         \declListBoundsM should have populated this)"

linearizeWithBounds :: [String] -> [String] -> String
linearizeWithBounds subs bounds =
  let n = length subs
      strides = [ concatMap (\j -> "(" ++ bounds !! j ++ ")" ++ " * ") [i+1..n-1]
                | i <- [0..n-1] ]
      terms = zipWith (\sub stride ->
        if null stride then sub
        else stride ++ sub) subs strides
  in intercalate " + " terms


-- ═══════════════════════════════════════════════════════════════════════
-- §7. Domain bound extraction (pure, constraint-based)
-- ═══════════════════════════════════════════════════════════════════════

-- | Attempt to extract an exclusive upper bound for dimension @dim@ from
-- a list of conjunctions using a structural pattern match.  Returns
-- 'Nothing' if no inequality matches the supported shapes; consumers
-- then fall back to ISL's @dim_max@.
extractOneBound :: [String] -> [C.Conjunction SetIx] -> Int -> Maybe String
extractOneBound params conjs dim =
  let ineqs = [ e | C.Conjunction cs <- conjs, InequalityConstraint e <- cs ]
  in case mapMaybe (matchUB dim params) ineqs of
    (ub:_) -> Just ub
    []     -> Nothing

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

-- | Local bound-extraction error.  Wrapped by callers into
-- 'Alpha.Codegen.CodegenError.BoundExtractionFailed' with the
-- variable name (which this module doesn't carry).
data BoundErr
  = BEPieceCount !Int     -- ^ @isl_pw_aff_n_piece@ returned @n /= 1@.
  | BEParseFailed !String -- ^ ISL @pw_aff@ string didn't match expected shape.
  deriving (Show, Eq)

instance NFData BoundErr where
  rnf (BEPieceCount n)  = rnf n
  rnf (BEParseFailed s) = rnf s

-- | ISL-powered bound extraction via @isl_set_dim_max@, running in
-- 'IslT' (no 'unsafePerformIO').  Piecewise bounds are rejected
-- structurally via @isl_pw_aff_n_piece@ — no substring heuristic.
--
-- Returns one upper bound per dimension.  'Left (dim, err)' on the
-- first failing dimension.
extractBoundsISLM
  :: String -> Int
  -> IslT IO (Ur (Either (Int, BoundErr) [String]))
extractBoundsISLM domStr nDims = Isl.do
  Ur results <- Isl.mapM (extractOneDimBoundM domStr) [0 .. nDims - 1]
  -- 'Isl.mapM' returns 'Ur [Ur (Either ...)]'; flatten the inner Ur
  -- wrappers before sequencing dims into a single 'Either'.
  let flat = [ e | Ur e <- results ]
  Isl.pure (Ur (sequenceDims flat))
  where
    sequenceDims :: [Either BoundErr String] -> Either (Int, BoundErr) [String]
    sequenceDims = go 0
      where
        go _ []                = Right []
        go d (Left err : _)    = Left (d, err)
        go d (Right b  : rest) = fmap (b :) (go (d + 1) rest)

-- | Extract an exclusive upper bound for one dimension, using
-- @dim_max + 1@.  Fails if the result is piecewise (@n_piece /= 1@)
-- or the printed form doesn't match the single-piece template.
extractOneDimBoundM
  :: String -> Int
  -> IslT IO (Ur (Either BoundErr String))
extractOneDimBoundM domStr d = Isl.do
  s <- RS.readFromStr domStr
  pa <- RS.dimMax s d
  one <- Val.intFromSi 1
  excl <- PA.addConstantVal pa one
  Both (Ur np) excl' <- query excl PA.nPiece
  if np /= 1
    then Isl.do
      freeM excl'
      Isl.pure (Ur (Left (BEPieceCount np)))
    else Isl.do
      Ur str <- query_ excl' PA.toStr
      Isl.pure (Ur (case extractPwAffExpr str of
                      Just expr -> Right expr
                      Nothing   -> Left (BEParseFailed str)))

-- | Extract the expression between @[(@…@)]@ in ISL pw_aff output.
-- Single-piece format: @[params] -> { [(expr)] }@.  Returns 'Nothing'
-- if the string doesn't match that template.
extractPwAffExpr :: String -> Maybe String
extractPwAffExpr = go
  where
    go ('{':' ':'[':'(':rest) = Just (takeUntilClose rest)
    go (_:rest) = go rest
    go [] = Nothing
    -- Match ")]" — handles nested parens (e.g., "floord(N, 2) + 1")
    takeUntilClose = reverse . drop 1 . dropWhile (/= ')') . reverse


-- ═══════════════════════════════════════════════════════════════════════
-- §8. Binary/unary op rendering
-- ═══════════════════════════════════════════════════════════════════════

renderBinOp :: String -> BinOp -> String -> String -> String
renderBinOp _   OpAdd a b = "(" ++ a ++ " + " ++ b ++ ")"
renderBinOp _   OpSub a b = "(" ++ a ++ " - " ++ b ++ ")"
renderBinOp _   OpMul a b = "(" ++ a ++ " * " ++ b ++ ")"
renderBinOp _   OpDiv a b = "(" ++ a ++ " / " ++ b ++ ")"
renderBinOp sfx OpMin a b = "fmin" ++ sfx ++ "(" ++ a ++ ", " ++ b ++ ")"
renderBinOp sfx OpMax a b = "fmax" ++ sfx ++ "(" ++ a ++ ", " ++ b ++ ")"

renderUnaryOp :: String -> UnaryOp -> String -> String
renderUnaryOp _   OpNeg   s = "(-(" ++ s ++ "))"
renderUnaryOp sfx OpAbs   s = "fabs" ++ sfx ++ "(" ++ s ++ ")"
renderUnaryOp sfx OpFloor s = "floor" ++ sfx ++ "(" ++ s ++ ")"
renderUnaryOp sfx OpCeil  s = "ceil" ++ sfx ++ "(" ++ s ++ ")"
renderUnaryOp sfx OpSqrt  s = "sqrt" ++ sfx ++ "(" ++ s ++ ")"


-- ═══════════════════════════════════════════════════════════════════════
-- §9. Domain condition rendering
-- ═══════════════════════════════════════════════════════════════════════

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
