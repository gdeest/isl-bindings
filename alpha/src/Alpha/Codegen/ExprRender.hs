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
  , renderExprToC
  , renderEquationMacro
  , renderCoreExprToC
  , extractSubscripts
  , extractOneBound
  , extractBoundsISLM
  , extractBoundsFromSetM
  , BoundErr(..)
  , RenderErr(..)
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
import GHC.TypeLits (KnownNat, Nat, Symbol, symbolVal, type (+))

import Isl.Typed.Constraints
  ( Conjunction(..), Constraint(..), MapIx(..), SetIx(..)
  , NamedMap(..), NamedSet(..) )
import qualified Isl.Typed.Constraints as C
import Isl.Typed.Params (KnownSymbols)
import Isl.TypeLevel.Reflection (DomTag, KnownDom, reflectDomConstraints, domToSet)
import Isl.TypeLevel.Sing (knownConstraints, reifySTConstraintsMapSplit)
import Isl.Monad (IslT, Ur(..))
import Isl.Linear (query, query_, freeM, Both(..))
import qualified Isl.Linear as Isl
import qualified Isl.Set as RS
import Isl.Types (Set)
import qualified Isl.PwAff as PA
import qualified Isl.Val as Val

import Alpha.Surface.Core
import qualified Alpha.Core    as Core
import qualified Alpha.Core.Named as Named
import Alpha.Core.Tokens (materializeNamedSet)
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

-- | Render an 'Expr' to a C expression string.  Returns 'Left' when
-- rendering surfaces a structured error (currently only
-- 'RENonStandardMapConstraint' from 'extractSubscripts').
renderExprToC
  :: forall ps pctx decls n (d :: DomTag ps n) a.
     RenderCtx -> Alpha.Surface.Core.Expr ps pctx decls n d a -> Either RenderErr String

renderExprToC ctx (Var (Proxy :: Proxy name)) =
  let varName = symbolVal (Proxy @name)
  in Right (renderArrayAccess varName (rcIterVars ctx) ctx)

renderExprToC _ctx (Const v) =
  -- The 'AlphaScalar' dict carried by 'Const' gives us the bridge
  -- directly — no 'rcDesc'-keyed lookup, no type-unsafe cast (#1).
  Right (showLiteral scalarConstBridge v)

renderExprToC ctx (Pw op e1 e2) = do
  let sfx = descMathSuffix (rcDesc ctx)
  a <- renderExprToC ctx e1
  b <- renderExprToC ctx e2
  pure (renderBinOp sfx op a b)

renderExprToC ctx (PMap op e) = do
  let sfx = descMathSuffix (rcDesc ctx)
  s <- renderExprToC ctx e
  pure (renderUnaryOp sfx op s)

renderExprToC ctx
  (Dep (Proxy :: Proxy mapCs) (inner :: Alpha.Surface.Core.Expr ps pctx decls no dInner a)) = do
  let ni  = length (rcIterVars ctx)
      cs  = reifySTConstraintsMapSplit ni
              (knownConstraints @ps @(n + no) @mapCs)
      -- Surface 'at' and 'Introduce.copyBody' both produce Dep _ (Var _).
      targetName = case inner of
        Var (Proxy :: Proxy nm) -> symbolVal (Proxy @nm)
        _ -> error "Alpha.Codegen.ExprRender: Dep wraps non-Var (invariant broken by a transform)"
  subs <- extractSubscripts ni (rcIterVars ctx) (rcParams ctx) targetName cs
  -- Inner variable bounds are precomputed at the top of codegen
  -- from the declared-variable domains (inputs/outputs/locals).
  -- No ISL calls happen here — the renderer is a pure function.
  let innerCtx = ctx { rcIterVars = subs }
  renderExprToC innerCtx inner

renderExprToC ctx (Reduce _rop _projCs inner) =
  renderExprToC ctx inner

renderExprToC ctx (Case branches) =
  renderBranches ctx branches

renderBranches
  :: forall ps pctx decls n (amb :: DomTag ps n) branchDoms a.
     RenderCtx -> Branches ps pctx decls n amb branchDoms a
  -> Either RenderErr String
renderBranches _ctx BNil = Right "0 /* unreachable */"
renderBranches ctx (BCons _ body BNil) =
  renderExprToC ctx body
renderBranches ctx (BCons (_ :: Proxy d) body rest) = do
  let condCs = reflectDomConstraints @ps @n @d
      condStr = renderDomCondition (rcIterVars ctx) (rcParams ctx) condCs
  b <- renderExprToC ctx body
  r <- renderBranches ctx rest
  pure $ "(" ++ condStr ++ ") ? (" ++ b ++ ") : (" ++ r ++ ")"


-- | Generate a C macro definition for one Core equation.  Polyhedral
-- payload (domain conjunctions, dependence map indexing) is read off
-- the 'Named' fields carried on each constructor; no plugin reflection.
renderEquationMacro
  :: forall sys d a.
     String       -- statement macro name
  -> String       -- logical array name
  -> Int          -- number of output dimensions
  -> Core.Expr sys d a
  -> RenderCtx
  -> Either RenderErr String
renderEquationMacro macroName lhsName nOutDims body ctx = do
  let nArgs   = length (rcIterVars ctx)
      formals = ["_a" ++ show i | i <- [0 .. nArgs - 1]]
      args    = intercalate "," formals
      writeVars = take nOutDims (rcIterVars ctx)
      writeLhs = renderArrayAccess lhsName writeVars ctx
  (accOp, bodyExpr) <- case body of
    Core.Reduce rop _namedP _namedBody _tok inner -> do
      innerStr <- renderCoreExprToC ctx inner
      pure (reduceOpToC rop (rcDesc ctx) writeLhs innerStr)
    _ -> do
      bodyStr <- renderCoreExprToC ctx body
      pure (" = ", bodyStr)
  pure $ "#define " ++ macroName ++ "(" ++ args ++ ") do { "
       ++ writeLhs ++ accOp ++ bodyExpr ++ "; } while(0)"

-- | Render a Core 'Alpha.Core.Expr' to a C expression string.
renderCoreExprToC
  :: forall sys d a.
     RenderCtx -> Core.Expr sys d a -> Either RenderErr String

renderCoreExprToC ctx (Core.Var pv _ _) =
  let varName = symbolVal pv
  in Right (renderArrayAccess varName (rcIterVars ctx) ctx)

renderCoreExprToC _ctx (Core.Const v) =
  Right (showLiteral scalarConstBridge v)

renderCoreExprToC ctx (Core.Pw op e1 e2) = do
  let sfx = descMathSuffix (rcDesc ctx)
  a <- renderCoreExprToC ctx e1
  b <- renderCoreExprToC ctx e2
  pure (renderBinOp sfx op a b)

renderCoreExprToC ctx (Core.PMap op e) = do
  let sfx = descMathSuffix (rcDesc ctx)
  s <- renderCoreExprToC ctx e
  pure (renderUnaryOp sfx op s)

renderCoreExprToC ctx (Core.Dep namedM _namedSrc _tok inner) = do
  let nm  = Named.the namedM
      ni  = nmNIn nm
      cs  = concat [ks | Conjunction ks <- nmConjs nm]
      targetName = case inner of
        Core.Var pv _ _ -> symbolVal pv
        _ -> error "Alpha.Codegen.ExprRender.renderCoreExprToC: \
                   \Dep wraps non-Var (invariant broken by a transform)"
  subs <- extractSubscripts ni (rcIterVars ctx) (rcParams ctx) targetName cs
  let innerCtx = ctx { rcIterVars = subs }
  renderCoreExprToC innerCtx inner

renderCoreExprToC ctx (Core.Reduce _rop _namedP _namedBody _tok inner) =
  renderCoreExprToC ctx inner

renderCoreExprToC ctx (Core.Case _partTok branches) =
  renderCoreBranches ctx branches

renderCoreBranches
  :: forall sys d bs a.
     RenderCtx -> Core.CaseBranches sys d bs a
  -> Either RenderErr String
renderCoreBranches _ctx Core.BNil = Right "0 /* unreachable */"
renderCoreBranches ctx (Core.BCons _ _ body Core.BNil) =
  renderCoreExprToC ctx body
renderCoreBranches ctx (Core.BCons namedB _ body rest) = do
  let bs       = Named.the namedB
      condCs   = concat [ks | Conjunction ks <- nsConjs bs]
      condStr  = renderDomCondition (rcIterVars ctx) (rcParams ctx) condCs
  b <- renderCoreExprToC ctx body
  r <- renderCoreBranches ctx rest
  pure $ "(" ++ condStr ++ ") ? (" ++ b ++ ") : (" ++ r ++ ")"


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Subscript extraction from affine map constraints
-- ═══════════════════════════════════════════════════════════════════════

-- | Module-local error; lifted to 'CodegenError' by the caller.
data RenderErr
  = RENonStandardMapConstraint !String !Int
  deriving (Show, Eq)

instance NFData RenderErr where
  rnf (RENonStandardMapConstraint v k) = rnf v `seq` rnf k

-- 'C.Expr' has no @Ord@ upstream and an orphan here would leak, so
-- 'FloorDiv' atoms are keyed by their 'Show' fingerprint — stable
-- because the reifier emits identical trees for identical existentials.
data AffKey
  = KMapIx !MapIx
  | KFloorDiv !String
  deriving (Eq, Ord, Show)

-- Atom expr is stored alongside the coefficient so 'coeffsToExpr' can
-- rebuild terms without an inverse lookup.
type AffCoeffs = Map AffKey (Integer, C.Expr MapIx)

-- 'FloorDiv' is opaque: keyed as an atom rather than reasoned through.
extractAffineCoeffs :: C.Expr MapIx -> (AffCoeffs, Integer)
extractAffineCoeffs = go 1
  where
    merge :: AffCoeffs -> AffCoeffs -> AffCoeffs
    merge = Map.unionWith (\(c1, e) (c2, _) -> (c1 + c2, e))

    go :: Integer -> C.Expr MapIx -> (AffCoeffs, Integer)
    go s (C.Constant n) = (Map.empty, s * n)
    go s (C.Ix ix)      = (Map.singleton (KMapIx ix) (s, C.Ix ix), 0)
    go s (C.Add a b)    =
      let (ca, ka) = go s a
          (cb, kb) = go s b
      in (merge ca cb, ka + kb)
    go s (C.Mul k e)    = go (s * k) e
    go s fd@(C.FloorDiv _ _) =
      (Map.singleton (KFloorDiv (show fd)) (s, fd), 0)

-- Keys are emitted in 'Ord' order for determinism.
coeffsToExpr :: AffCoeffs -> Integer -> C.Expr MapIx
coeffsToExpr coeffs k0 =
  let terms = [ mkTerm c expr
              | (_, (c, expr)) <- Map.toAscList coeffs, c /= 0 ]
      allTerms = if k0 == 0 then terms else terms ++ [C.Constant k0]
  in case allTerms of
    []     -> C.Constant 0
    (x:xs) -> foldl C.Add x xs
  where
    mkTerm 1    e = e
    mkTerm (-1) e = C.Mul (-1) e
    mkTerm c    e = C.Mul c e

-- One subscript per output dim, derived from equality constraints.
-- Non-±1 coefficients on @OutDim k@ are reported, not silently dropped.
extractSubscripts
  :: Int                        -- number of input dims
  -> [String]                   -- input dim variable names
  -> [String]                   -- parameter names
  -> String                     -- target variable name (for error reports)
  -> [Constraint MapIx]         -- map constraints
  -> Either RenderErr [String]  -- one C expression per output dim
extractSubscripts _ni iterVars paramNames targetName cs = do
  pairs <- sequence (mapMaybe extractEquality cs)
  Right (map snd (sortBy (comparing fst) pairs))
  where
    extractEquality
      :: Constraint MapIx -> Maybe (Either RenderErr (Int, String))
    extractEquality (EqualityConstraint expr) =
      let (coeffs, kConst) = extractAffineCoeffs expr
          outs = [ (k, c)
                 | (KMapIx (OutDim k), (c, _)) <- Map.toAscList coeffs
                 , c /= 0 ]
      in
      case outs of
        [] -> Nothing
        ((firstK, _):_:_) ->
          Just (Left (RENonStandardMapConstraint targetName firstK))
        [(outIdx, c)]
          | c == 1 ->
              -- OutDim k + rest = 0  ⇒  OutDim k = -rest
              let bodyCoeffs = Map.delete (KMapIx (OutDim outIdx)) coeffs
                  negCoeffs  = Map.map (\(x, e) -> (negate x, e)) bodyCoeffs
                  bodyExpr   = coeffsToExpr negCoeffs (negate kConst)
                  rendered   = renderMapExpr iterVars paramNames bodyExpr
              in Just (Right (outIdx, rendered))
          | c == -1 ->
              -- -OutDim k + rest = 0  ⇒  OutDim k = rest
              let bodyCoeffs = Map.delete (KMapIx (OutDim outIdx)) coeffs
                  bodyExpr   = coeffsToExpr bodyCoeffs kConst
                  rendered   = renderMapExpr iterVars paramNames bodyExpr
              in Just (Right (outIdx, rendered))
          | otherwise ->
              Just (Left (RENonStandardMapConstraint targetName outIdx))
    extractEquality _ = Nothing


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
      -- Parenthesize every sub: after Case-split fan-out, a pinned
      -- branch dim can surface as "N - 1" or similar, and
      -- "(N) * (N) * N - 1" parses as "((N*N*N) - 1)", not
      -- "(N*N)*(N-1)".  Plain loop-var subs like "c0" are unaffected.
      terms = zipWith (\sub stride ->
        if null stride then sub
        else stride ++ "(" ++ sub ++ ")") subs strides
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
-- first failing dimension.  The domain is materialised from its
-- 'KnownDom' dictionary via 'domToSet' — a fresh 'Isl.Set' is built
-- per dim (mirrors the prior one-set-per-'readFromStr' shape).
extractBoundsISLM
  :: forall (ps :: [Symbol]) (n :: Nat) (d :: DomTag ps n).
     ( KnownDom ps n d
     , KnownSymbols ps
     , KnownNat n
     )
  => Int
  -> IslT IO (Ur (Either (Int, BoundErr) [String]))
extractBoundsISLM nDims = Isl.do
  Ur results <- Isl.mapM (extractOneDimBoundM @ps @n @d) [0 .. nDims - 1]
  -- 'Isl.mapM' returns 'Ur [Ur (Either ...)]'; flatten the inner Ur
  -- wrappers before sequencing dims into a single 'Either'.
  let flat = [ e | Ur e <- results ]
  Isl.pure (Ur (sequenceDims flat))
  where
    sequenceDims :: [Either BoundErr String] -> Either (Int, BoundErr) [String]
    sequenceDims = go 0
      where
        go _ []                = Right []
        go d' (Left err : _)    = Left (d', err)
        go d' (Right b  : rest) = fmap (b :) (go (d' + 1) rest)

-- | Extract an exclusive upper bound for one dimension, using
-- @dim_max + 1@.  Fails if the result is piecewise (@n_piece /= 1@)
-- or the printed form doesn't match the single-piece template.
extractOneDimBoundM
  :: forall (ps :: [Symbol]) (n :: Nat) (d :: DomTag ps n).
     ( KnownDom ps n d
     , KnownSymbols ps
     , KnownNat n
     )
  => Int
  -> IslT IO (Ur (Either BoundErr String))
extractOneDimBoundM d = Isl.do
  s <- domToSet @ps @n @d
  extractFromSetM s d

-- | Bound extractor over a runtime 'NamedSet' (carried on
-- 'Alpha.Core.VarDecl.vdDom').  Mirrors 'extractBoundsISLM' but with
-- no type-level reflection — the domain is materialised from its
-- 'Named' payload.
extractBoundsFromSetM
  :: NamedSet
  -> Int
  -> IslT IO (Ur (Either (Int, BoundErr) [String]))
extractBoundsFromSetM ns nDims = Isl.do
  Ur results <- Isl.mapM (extractOneFromNamedSetM ns) [0 .. nDims - 1]
  let flat = [ e | Ur e <- results ]
  Isl.pure (Ur (sequenceDims flat))
  where
    sequenceDims :: [Either BoundErr String] -> Either (Int, BoundErr) [String]
    sequenceDims = go 0
      where
        go _ []                 = Right []
        go d' (Left err : _)    = Left (d', err)
        go d' (Right b  : rest) = fmap (b :) (go (d' + 1) rest)

extractOneFromNamedSetM
  :: NamedSet -> Int -> IslT IO (Ur (Either BoundErr String))
extractOneFromNamedSetM ns d = Isl.do
  s <- materializeNamedSet ns
  extractFromSetM s d

extractFromSetM :: Set %1 -> Int -> IslT IO (Ur (Either BoundErr String))
extractFromSetM s d = Isl.do
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
