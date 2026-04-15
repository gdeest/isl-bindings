{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Lazy on-demand interpreter for Alpha systems.
--
-- This module provides a concrete, memoized interpreter that evaluates
-- Alpha recurrence equations at specific integer points.  It is a
-- sanity checker, not a codegen pass: it walks the 'Expr' GADT
-- directly, evaluating operations at each point and caching results
-- in a 'Map'.
--
-- Usage:
--
-- @
-- eval <- interpret matmul
--           (Map.fromList [(\"N\", 3)])
--           (Map.fromList [(\"A\", \\[i,j] -> a V.! (i*3+j)),
--                          (\"B\", \\[i,j] -> b V.! (i*3+j))])
-- val <- eval \"C\" [1, 2]  -- evaluates C[1,2] on demand
-- @
module Alpha.Interpret
  ( interpret
  ) where

import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Foreign.C (newCString)
import GHC.TypeLits (natVal, symbolVal, type (+))
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.Typed.Constraints (SetIx(..))
import qualified Isl.Typed.Constraints as C
import Isl.Typed.Params (KnownSymbols, symbolVals)
import qualified Isl.Set as RS
import Isl.Types
  ( Ctx(..)
  , c_ctx_alloc, c_ctx_free
  , islDimParam, islDimSet
  )
import qualified Isl.Types as IT
import Isl.TypeLevel.Reflection (DomTag, EffectiveDomTag, reflectDomString)
import Isl.TypeLevel.Sing
  ( knownConstraints, reifySTConstraintsSet )

import Alpha.Core


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Public API
-- ═══════════════════════════════════════════════════════════════════════

-- | Interpret a system at concrete parameter values.
--
-- Returns a memoized lookup function: given a variable name and an
-- integer point, evaluates the recurrence equation lazily (caching
-- results for future lookups).
--
-- Input variables are provided as pure functions from integer
-- coordinates to 'Double'.  Output and local variables are evaluated
-- on demand from their equations.
interpret
  :: forall ps inputs outputs locals.
     ( KnownSymbols ps )
  => System ps inputs outputs locals
  -> Map String Int                -- ^ parameter name → concrete value
  -> Map String ([Int] -> Double)  -- ^ input variable name → point accessor
  -> IO (String -> [Int] -> IO Double)
interpret (System decls eqs) params inputFns = do
  -- Validate: all params provided
  let paramNames = symbolVals @ps
  mapM_ (\p -> case Map.lookup p params of
    Nothing -> error $ "Alpha.Interpret: missing parameter " ++ show p
    Just _  -> pure ()
    ) paramNames
  -- Validate: all inputs provided
  let inputNames = declListNames (dInputs decls)
  mapM_ (\n -> case Map.lookup n inputFns of
    Nothing -> error $ "Alpha.Interpret: missing input " ++ show n
    Just _  -> pure ()
    ) inputNames
  -- Build environment
  cacheRef <- newIORef Map.empty
  let maxParam = if Map.null params then 0
                 else maximum (Map.elems params)
      env = Env
        { envParams    = params
        , envParamVec  = paramNames
        , envMaxParam  = maxParam
        , envInputs    = inputFns
        , envCache     = cacheRef
        , envLookupEq  = \name pt -> evalEquation env eqs name pt
        }
  pure $ \varName point ->
    case Map.lookup varName inputFns of
      Just f  -> pure (f point)
      Nothing -> lookupCached env varName point


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Environment
-- ═══════════════════════════════════════════════════════════════════════

data Env = Env
  { envParams    :: !(Map String Int)
  , envParamVec  :: ![String]           -- param names in positional order
  , envMaxParam  :: !Int                -- max of all param values
  , envInputs    :: !(Map String ([Int] -> Double))
  , envCache     :: !(IORef (Map String (Map [Int] Double)))
  , envLookupEq  :: !(String -> [Int] -> IO Double)
  }


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Memoized variable lookup
-- ═══════════════════════════════════════════════════════════════════════

lookupCached :: Env -> String -> [Int] -> IO Double
lookupCached env varName point = do
  cache <- readIORef (envCache env)
  case Map.lookup varName cache >>= Map.lookup point of
    Just v  -> pure v
    Nothing -> do
      v <- envLookupEq env varName point
      modifyIORef' (envCache env) $
        Map.insertWith Map.union varName (Map.singleton point v)
      pure v

evalEquation
  :: forall ps decls defined.
     Env
  -> EqList ps decls defined
  -> String -> [Int] -> IO Double
evalEquation _   EqNil target _ =
  error $ "Alpha.Interpret: no equation for variable " ++ show target
evalEquation env (Defines (Proxy :: Proxy name) body :& rest) target point
  | symbolVal (Proxy @name) == target =
      -- body :: Expr ps decls n d a  where a = DeclType decl
      -- All current examples use a ~ Double; unsafeCoerce at boundary.
      unsafeCoerce (evalExpr env body point)
  | otherwise =
      evalEquation env rest target point


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Expression evaluator
-- ═══════════════════════════════════════════════════════════════════════

evalExpr
  :: forall ps decls n (d :: DomTag ps n) a.
     Env -> Expr ps decls n d a -> [Int] -> IO a

evalExpr env (Var (Proxy :: Proxy name)) point = do
  let varName = symbolVal (Proxy @name)
  case Map.lookup varName (envInputs env) of
    Just f  -> pure (unsafeCoerce (f point) :: a)
    Nothing -> unsafeCoerce (lookupCached env varName point)

evalExpr _ (Const v) _ = pure v

evalExpr env (Pw f e1 e2) point = do
  !v1 <- evalExpr env e1 point
  !v2 <- evalExpr env e2 point
  pure $! f v1 v2

evalExpr env (PMap f e) point = do
  !v <- evalExpr env e point
  pure $! f v

evalExpr env (Dep (Proxy :: Proxy mapCs)
                  (inner :: Expr ps decls no dInner a)) point = do
  let niVal = fromIntegral (natVal (Proxy @n)) :: Int
      cs = reifySTConstraintsSet (knownConstraints @ps @(n + no) @mapCs)
      innerPoint = applyAffineMap niVal (envParams env) (envParamVec env) cs point
  evalExpr env inner innerPoint

evalExpr env (Reduce (Proxy :: Proxy projCs)
                     (body :: Expr ps decls nBody dBody m)) point = do
  let nVal     = fromIntegral (natVal (Proxy @n)) :: Int
      nBodyVal = fromIntegral (natVal (Proxy @nBody)) :: Int
      nRed  = nBodyVal - nVal
      bodyDomStr = reflectDomString @ps @nBody @dBody
      maxVal = envMaxParam env
      -- Enumerate all candidate reduction variable combinations
      redRanges = replicate nRed [0 .. maxVal]
      candidates = sequence redRanges
  -- Filter: keep only body points in the body domain
  let validBodyPoints =
        [ point ++ red
        | red <- candidates
        , islMember (envParams env) (envParamVec env) nBodyVal bodyDomStr (point ++ red)
        ]
  -- Evaluate body at each valid point and fold via Monoid
  vals <- mapM (evalExpr env body) validBodyPoints
  pure $! mconcat vals

evalExpr env (Case branches) point =
  evalBranches env branches point


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Branch evaluation
-- ═══════════════════════════════════════════════════════════════════════

evalBranches
  :: forall ps decls n (amb :: DomTag ps n) branchDoms a.
     Env
  -> Branches ps decls n amb branchDoms a
  -> [Int] -> IO a
evalBranches _ BNil point =
  error $ "Alpha.Interpret: no branch matches point " ++ show point
evalBranches env (BCons (_ :: Proxy d) body rest) point = do
  let effDomStr = reflectDomString @ps @n @(EffectiveDomTag d amb)
      nDims = length point
  if islMember (envParams env) (envParamVec env) nDims effDomStr point
    then evalExpr env body point
    else evalBranches env rest point


-- ═══════════════════════════════════════════════════════════════════════
-- §6. Affine map evaluation (pure Haskell)
-- ═══════════════════════════════════════════════════════════════════════

-- | Apply an affine map (encoded as equality constraints in the joined
-- ni+no space) to an input point, producing the output point.
applyAffineMap
  :: Int                    -- ^ ni (number of input dims)
  -> Map String Int         -- ^ parameter values
  -> [String]               -- ^ parameter names in positional order
  -> [C.Constraint SetIx]     -- ^ map constraints in the joined space
  -> [Int]                  -- ^ input point
  -> [Int]                  -- ^ output point
applyAffineMap ni params paramNames cs inputPoint =
  -- For IslMultiAffToMap constraints: each equality defines exactly
  -- one output dim.  The constraints are in the joined ni+no space
  -- where dims 0..ni-1 are input and ni..ni+no-1 are output.
  --
  -- Strategy: for each equality, evaluate with output dims = 0.
  -- The result is -f(inputs, params).  The output dim has coefficient 1
  -- (guaranteed by IslMultiAffToMap normalization), so output = -result.
  let paramLookup p
        | p >= 0 && p < length paramNames =
            Map.findWithDefault 0 (paramNames !! p) params
        | otherwise = 0

      dimLookup d
        | d < ni    = inputPoint !! d
        | otherwise = 0  -- output dim = 0 for evaluation

      -- Collect output dim → value from equalities
      outputMap = Map.fromList
        [ (outIdx, outVal)
        | C.EqualityConstraint expr <- cs
          -- Evaluate with all output dims = 0
        , let val0 = evalAff dimLookup paramLookup expr
          -- Find which output dim has non-zero coefficient
          -- by evaluating with that dim = 1
        , outIdx <- [ni .. ni + length inputPoint]
        , let dimLookup1 d = if d == outIdx then 1 else dimLookup d
              val1 = evalAff dimLookup1 paramLookup expr
              coeff = val1 - val0
        , coeff /= 0
        , let outVal = negate val0 `div` coeff
        ]

      no = if Map.null outputMap then 0
           else maximum (Map.keys outputMap) - ni + 1
  in [ Map.findWithDefault 0 (ni + j) outputMap | j <- [0 .. no - 1] ]

-- | Evaluate an affine expression with concrete dim and param values.
evalAff :: (Int -> Int) -> (Int -> Int) -> C.Expr SetIx -> Int
evalAff dimVal paramVal = go
  where
    go (C.Ix (SetDim d))   = dimVal d
    go (C.Ix (SetParam p)) = paramVal p
    go (C.Constant n')     = fromIntegral n'
    go (C.Mul k e)         = fromIntegral k * go e
    go (C.Add a b)         = go a + go b
    go (C.FloorDiv e d)    = go e `div` fromIntegral d


-- ═══════════════════════════════════════════════════════════════════════
-- §7. ISL-backed domain membership testing
-- ═══════════════════════════════════════════════════════════════════════

-- | Test whether a point lies in a domain described by an ISL string.
--
-- Uses the ISL C library directly via 'unsafePerformIO'.
islMember
  :: Map String Int  -- ^ parameter values
  -> [String]        -- ^ parameter names in positional order
  -> Int             -- ^ number of dimensions
  -> String          -- ^ ISL domain string (from 'reflectDomString')
  -> [Int]           -- ^ point coordinates
  -> Bool
islMember params paramNames _nDims domStr point = unsafePerformIO $ do
  ctxPtr <- c_ctx_alloc
  let ctx = Ctx ctxPtr
  domCStr <- newCString domStr
  set0 <- RS.c_readFromStr ctx domCStr
  -- Fix each parameter to its concrete value
  set1 <- fixAllParams (0 :: Int) paramNames params set0
  -- Fix each dimension to the point's coordinate
  set2 <- fixAllDims (0 :: Int) point set1
  -- isEmpty uses __isl_keep (borrowed ref), safe to call then free
  let !result = not (RS.isEmpty (IT.SetRef (IT.unSet set2)))
  RS.c_free set2
  c_ctx_free ctxPtr
  pure result
{-# NOINLINE islMember #-}

fixAllParams :: Int -> [String] -> Map String Int -> IT.Set -> IO IT.Set
fixAllParams _ [] _ s = pure s
fixAllParams i (p:ps) params s = case Map.lookup p params of
  Just v  -> do
    s' <- RS.c_fixSi s islDimParam (fromIntegral i) (fromIntegral v)
    fixAllParams (i + 1) ps params s'
  Nothing -> fixAllParams (i + 1) ps params s

fixAllDims :: Int -> [Int] -> IT.Set -> IO IT.Set
fixAllDims _ [] s = pure s
fixAllDims i (v:vs) s = do
  s' <- RS.c_fixSi s islDimSet (fromIntegral i) (fromIntegral v)
  fixAllDims (i + 1) vs s'


-- ═══════════════════════════════════════════════════════════════════════
-- §8. Declaration list traversal
-- ═══════════════════════════════════════════════════════════════════════

declListNames :: forall ps decls. DeclList ps decls -> [String]
declListNames Nil = []
declListNames ((MkDecl :: Decl ps d) :> rest) =
  symbolVal (Proxy @(DeclName d)) : declListNames rest
