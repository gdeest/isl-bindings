{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Lazy on-demand interpreter for Alpha systems.
--
-- Usage:
--
-- @
-- eval <- interpret \@Double matmul
--           (Map.fromList [(\"N\", 3)])
--           (Map.fromList [(\"A\", \\[i,j] -> a V.! (i*3+j)),
--                          (\"B\", \\[i,j] -> b V.! (i*3+j))])
-- val <- eval \"C\" [1, 2]
-- @
module Alpha.Interpret
  ( interpret
  ) where

import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, Nat, Symbol, natVal, symbolVal, type (+))
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.Typed.Constraints (SetIx(..))
import qualified Isl.Typed.Constraints as C
import Isl.Typed.Params (KnownSymbols, symbolVals)
import qualified Isl.Set as RS
import qualified Isl.Types as IT
import Isl.Monad (IslT, runIslT, Ur(..))
import Isl.Linear (query_)
import qualified Isl.Linear as Isl
import Isl.TypeLevel.Reflection (DomTag, EffectiveDomTag, KnownDom, domToSet, checkParamCtx)
import Isl.TypeLevel.Sing
  ( KnownConstraints, knownConstraints, reifySTConstraintsSet )

import Alpha.Core
import Alpha.Scalar
  ( ScalarDesc(..), AlphaScalar, scalarDesc
  , HsInterp(..), evalBinOp, evalUnaryOp, evalReduceOp )


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Public API
-- ═══════════════════════════════════════════════════════════════════════

-- | Interpret a system at concrete parameter values.
interpret
  :: forall a ps pctx inputs outputs locals.
     ( AlphaScalar a, KnownSymbols ps
     , KnownConstraints ps 0 pctx )
  => System ps pctx inputs outputs locals
  -> Map String Int                -- ^ parameter name → concrete value
  -> Map String ([Int] -> a)       -- ^ input variable name → point accessor
  -> IO (String -> [Int] -> IO a)
interpret (System decls eqs) params inputFns = do
  let paramNames = symbolVals @ps
  mapM_ (\p -> case Map.lookup p params of
    Nothing -> error $ "Alpha.Interpret: missing parameter " ++ show p
    Just _  -> pure ()
    ) paramNames
  case checkParamCtx @ps @pctx params of
    Left msg -> error $ "Alpha.Interpret: parameter context violated: " ++ msg
    Right () -> pure ()
  let inputNames = declListNames (dInputs decls)
  mapM_ (\n -> case Map.lookup n inputFns of
    Nothing -> error $ "Alpha.Interpret: missing input " ++ show n
    Just _  -> pure ()
    ) inputNames
  let erasedInputs = Map.map (\f -> \pt -> toCarrier (f pt)) inputFns
  -- All variables share the same ScalarDesc
  let desc = scalarDesc @a
      allNames = declListNames (dInputs decls)
              ++ declListNames (dOutputs decls)
              ++ declListNames (dLocals decls)
      descs = Map.fromList [(n, desc) | n <- allNames]
  cacheRef <- newIORef Map.empty
  let maxParam = if Map.null params then 0
                 else maximum (Map.elems params)
      env = Env
        { envParams    = params
        , envParamVec  = paramNames
        , envMaxParam  = maxParam
        , envInputs    = erasedInputs
        , envCache     = cacheRef
        , envDescs     = descs
        , envLookupEq  = \name pt -> evalEquation env eqs name pt
        }
  pure $ \varName point ->
    case Map.lookup varName erasedInputs of
      Just f  -> pure (fromCarrier (f point))
      Nothing -> do
        v <- lookupCached env varName point
        pure (fromCarrier v)


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Environment
-- ═══════════════════════════════════════════════════════════════════════

-- Type-erased box. Safe for any lifted type: store via MkCarrier,
-- recover via unsafeCoerce on the existential payload.
data Carrier = forall a. MkCarrier !a

toCarrier :: a -> Carrier
toCarrier = MkCarrier
{-# INLINE toCarrier #-}

fromCarrier :: Carrier -> a
fromCarrier (MkCarrier x) = unsafeCoerce x
{-# INLINE fromCarrier #-}

data Env = Env
  { envParams    :: !(Map String Int)
  , envParamVec  :: ![String]
  , envMaxParam  :: !Int
  , envInputs    :: !(Map String ([Int] -> Carrier))
  , envCache     :: !(IORef (Map String (Map [Int] Carrier)))
  , envDescs     :: !(Map String ScalarDesc)
  , envLookupEq  :: !(String -> [Int] -> IO Carrier)
  }


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Memoized variable lookup
-- ═══════════════════════════════════════════════════════════════════════

lookupCached :: Env -> String -> [Int] -> IO Carrier
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
  :: forall ps pctx decls defined.
     KnownSymbols ps
  => Env
  -> EqList ps pctx decls defined
  -> String -> [Int] -> IO Carrier
evalEquation _   EqNil target _ =
  error $ "Alpha.Interpret: no equation for variable " ++ show target
evalEquation env (Defines (Proxy :: Proxy name) body :& rest) target point
  | symbolVal (Proxy @name) == target =
      let desc = case Map.lookup target (envDescs env) of
            Just d  -> d
            Nothing -> error $ "Alpha.Interpret: no ScalarDesc for " ++ show target
      in do { !v <- evalExpr desc env body point; pure (toCarrier v) }
  | otherwise =
      evalEquation env rest target point


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Expression evaluator
-- ═══════════════════════════════════════════════════════════════════════

evalExpr
  :: forall ps pctx decls n (d :: DomTag ps n) a.
     KnownSymbols ps
  => ScalarDesc -> Env -> Expr ps pctx decls n d a -> [Int] -> IO a

evalExpr _desc env (Var (Proxy :: Proxy name)) point = do
  let varName = symbolVal (Proxy @name)
  case Map.lookup varName (envInputs env) of
    Just f  -> pure (fromCarrier (f point))
    Nothing -> do !v <- lookupCached env varName point
                  pure (fromCarrier v)

evalExpr _desc _ (Const v) _ = pure v

evalExpr desc env (Pw op e1 e2) point = do
  !v1 <- evalExpr desc env e1 point
  !v2 <- evalExpr desc env e2 point
  case desc of
    MkScalarDesc { sdHsInterp = Just hi } ->
      pure $! unsafeCoerce (evalBinOp hi op (unsafeCoerce v1) (unsafeCoerce v2))
    _ -> error "Alpha.Interpret: no HsInterp for this scalar type"

evalExpr desc env (PMap op e) point = do
  !v <- evalExpr desc env e point
  case desc of
    MkScalarDesc { sdHsInterp = Just hi } ->
      pure $! unsafeCoerce (evalUnaryOp hi op (unsafeCoerce v))
    _ -> error "Alpha.Interpret: no HsInterp for this scalar type"

evalExpr desc env (Dep (Proxy :: Proxy mapCs)
                  (inner :: Expr ps pctx decls no dInner a)) point = do
  let niVal = fromIntegral (natVal (Proxy @n)) :: Int
      cs = reifySTConstraintsSet (knownConstraints @ps @(n + no) @mapCs)
      innerPoint = applyAffineMap niVal (envParams env) (envParamVec env) cs point
  evalExpr desc env inner innerPoint

evalExpr desc env (Reduce reduceOp (Proxy :: Proxy projCs)
                     (body :: Expr ps pctx decls nBody dBody a)) point = do
  let nVal     = fromIntegral (natVal (Proxy @n)) :: Int
      nBodyVal = fromIntegral (natVal (Proxy @nBody)) :: Int
      nRed  = nBodyVal - nVal
      maxVal = envMaxParam env
      redRanges = replicate nRed [0 .. maxVal]
      candidates = sequence redRanges
  let validBodyPoints =
        [ point ++ red
        | red <- candidates
        , islMember @ps @nBody @dBody (envParams env) (envParamVec env) (point ++ red)
        ]
  vals <- mapM (evalExpr desc env body) validBodyPoints
  case desc of
    MkScalarDesc { sdHsInterp = Just hi } ->
      let typed = map unsafeCoerce vals
          step = evalReduceOp hi reduceOp
          result = case reduceOp of
            ReduceSum  -> foldl' step (hiFromInteger hi 0) typed
            ReduceProd -> foldl' step (hiFromInteger hi 1) typed
            ReduceMin  -> strictFoldl1 step typed
            ReduceMax  -> strictFoldl1 step typed
      in pure $! unsafeCoerce result
    _ -> error "Alpha.Interpret: no HsInterp for this scalar type"

evalExpr desc env (Case branches) point =
  evalBranches desc env branches point


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Branch evaluation
-- ═══════════════════════════════════════════════════════════════════════

evalBranches
  :: forall ps pctx decls n (amb :: DomTag ps n) branchDoms a.
     (KnownSymbols ps, KnownNat n)
  => ScalarDesc -> Env
  -> Branches ps pctx decls n amb branchDoms a
  -> [Int] -> IO a
evalBranches _ _ BNil point =
  error $ "Alpha.Interpret: no branch matches point " ++ show point
evalBranches desc env (BCons (_ :: Proxy d) body rest) point =
  if islMember @ps @n @(EffectiveDomTag d amb)
       (envParams env) (envParamVec env) point
    then evalExpr desc env body point
    else evalBranches desc env rest point


strictFoldl1 :: (a -> a -> a) -> [a] -> a
strictFoldl1 _ []     = error "Alpha.Interpret: reduction over empty domain"
strictFoldl1 f (x:xs) = foldl' f x xs


-- ═══════════════════════════════════════════════════════════════════════
-- §6. Affine map evaluation (pure Haskell)
-- ═══════════════════════════════════════════════════════════════════════

applyAffineMap
  :: Int -> Map String Int -> [String]
  -> [C.Constraint SetIx] -> [Int] -> [Int]
applyAffineMap ni params paramNames cs inputPoint =
  let paramLookup p
        | p >= 0 && p < length paramNames =
            Map.findWithDefault 0 (paramNames !! p) params
        | otherwise = 0
      dimLookup d
        | d < ni    = inputPoint !! d
        | otherwise = 0
      outputMap = Map.fromList
        [ (outIdx, outVal)
        | C.EqualityConstraint expr <- cs
        , let val0 = evalAff dimLookup paramLookup expr
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

-- | Test whether @point@ lies in the domain tagged by @d@, with
-- @params@ plugged in for parameter names.  Built on the high-level
-- linear bindings: 'domToSet' yields the raw set, 'RS.fixSi' nails the
-- parameters and coordinates, 'RS.isEmpty' decides non-membership.
-- No manual context or C-string plumbing.
islMember
  :: forall (ps :: [Symbol]) (n :: Nat) (d :: DomTag ps n).
     ( KnownDom ps n d
     , KnownSymbols ps
     , KnownNat n
     )
  => Map String Int -> [String] -> [Int] -> Bool
islMember params paramNames point = unsafePerformIO $ runIslT $ Isl.do
  s0 <- domToSet @ps @n @d
  s1 <- fixAllParams 0 paramNames params s0
  s2 <- fixAllDims 0 point s1
  Ur empty <- query_ s2 RS.isEmpty
  Isl.pure (Ur (not empty))
{-# NOINLINE islMember #-}

fixAllParams
  :: Int -> [String] -> Map String Int -> IT.Set %1 -> IslT IO IT.Set
fixAllParams _ [] _ s = Isl.pure s
fixAllParams i (p:ps') paramsMap s = case Map.lookup p paramsMap of
  Just v  -> Isl.do
    s' <- RS.fixSi s IT.islDimParam i v
    fixAllParams (i + 1) ps' paramsMap s'
  Nothing -> fixAllParams (i + 1) ps' paramsMap s

fixAllDims :: Int -> [Int] -> IT.Set %1 -> IslT IO IT.Set
fixAllDims _ [] s = Isl.pure s
fixAllDims i (v:vs) s = Isl.do
  s' <- RS.fixSi s IT.islDimSet i v
  fixAllDims (i + 1) vs s'


-- ═══════════════════════════════════════════════════════════════════════
-- §8. Declaration list traversal
-- ═══════════════════════════════════════════════════════════════════════

declListNames :: forall ps decls. DeclList ps decls -> [String]
declListNames Nil = []
declListNames ((MkDecl :: Decl ps d) :> rest) =
  symbolVal (Proxy @(DeclName d)) : declListNames rest
