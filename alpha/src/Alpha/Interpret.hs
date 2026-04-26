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
-- The interpreter takes a surface system, elaborates it internally
-- (TrustPlugin), and returns a closure that evaluates any declared
-- variable at a concrete point.  Values are memoised across calls.
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
import GHC.TypeLits (symbolVal)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.Typed.Constraints
  ( Conjunction(..), Constraint(..), MapIx(..)
  , NamedMap(..), NamedSet(..) )
import qualified Isl.Typed.Constraints as TC
import qualified Isl.Set as RS
import qualified Isl.Types as IT
import Isl.Monad (IslT, runIslT, Ur(..))
import Isl.Linear (query_)
import qualified Isl.Linear as Isl
import Isl.Typed.Params (KnownSymbols)

import Alpha.Core
  ( CaseBranches(..), Expr(..), ReduceOp(..)
  , SomeEquation(..), SomeVarDecl(..)
  , System(..), VarDecl(..) )
import Alpha.Core.Tokens (materializeNamedSet)
import qualified Alpha.Core.Named as Named
import qualified Alpha.Surface.Core as Surface
import Alpha.Surface.Elaborate (elaborate, ElabMode(..))
import Alpha.Scalar
  ( ScalarDesc(..), AlphaScalar, scalarDesc
  , HsInterp(..), evalBinOp, evalUnaryOp, evalReduceOp )


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Public API
-- ═══════════════════════════════════════════════════════════════════════

-- | Interpret a system at concrete parameter values.
--
-- Elaborates the surface system internally (TrustPlugin); an
-- elaboration failure is raised as a runtime 'error' to match the
-- @IO@ result type.
interpret
  :: forall a ps pctx inputs outputs locals.
     (AlphaScalar a, KnownSymbols ps)
  => Surface.System ps pctx inputs outputs locals
  -> Map String Int                -- ^ parameter name → concrete value
  -> Map String ([Int] -> a)       -- ^ input variable name → point accessor
  -> IO (String -> [Int] -> IO a)
interpret surfaceSys params inputFns =
  elaborate @ps @pctx @inputs @outputs @locals @a TrustPlugin surfaceSys $ \r -> case r of
    Left e    -> error ("Alpha.Interpret: " ++ show e)
    Right sys -> interpretCore sys params inputFns

interpretCore
  :: forall a sys.
     AlphaScalar a
  => System sys a
  -> Map String Int
  -> Map String ([Int] -> a)
  -> IO (String -> [Int] -> IO a)
interpretCore sys params inputFns = do
  let paramNames = Named.the (sysParams sys)
      pctxNS     = Named.the (sysParamCs sys)
  mapM_ (\p -> case Map.lookup p params of
    Nothing -> error $ "Alpha.Interpret: missing parameter " ++ show p
    Just _  -> pure ()
    ) paramNames
  case checkPctx paramNames pctxNS params of
    Left msg -> error $ "Alpha.Interpret: parameter context violated: " ++ msg
    Right () -> pure ()
  let inputNames = map varDeclName (sysInputs sys)
  mapM_ (\n -> case Map.lookup n inputFns of
    Nothing -> error $ "Alpha.Interpret: missing input " ++ show n
    Just _  -> pure ()
    ) inputNames
  let erasedInputs = Map.map (\f -> \pt -> toCarrier (f pt)) inputFns
  let desc = scalarDesc @a
      allNames = map varDeclName (sysInputs sys)
              ++ map varDeclName (sysOutputs sys)
              ++ map varDeclName (sysLocals sys)
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
        , envLookupEq  = \name pt -> evalEquationList env (sysEqs sys) name pt
        }
  pure $ \varName point ->
    case Map.lookup varName erasedInputs of
      Just f  -> pure (fromCarrier (f point))
      Nothing -> do
        v <- lookupCached env varName point
        pure (fromCarrier v)


varDeclName :: SomeVarDecl sys -> String
varDeclName (SomeVarDecl _ vd) = vdName vd


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Environment
-- ═══════════════════════════════════════════════════════════════════════

-- Type-erased existential box for the per-name cache.
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

evalEquationList
  :: forall sys a.
     AlphaScalar a
  => Env -> [SomeEquation sys a] -> String -> [Int] -> IO Carrier
evalEquationList _   []     target _ =
  error $ "Alpha.Interpret: no equation for variable " ++ show target
evalEquationList env (SomeEquation (_vd :: VarDecl sys v dom) body : rest)
                  target point
  | symbolVal (Proxy @v) == target =
      let desc = case Map.lookup target (envDescs env) of
            Just d  -> d
            Nothing -> error $ "Alpha.Interpret: no ScalarDesc for " ++ show target
      in do { !v <- evalExpr desc env body point; pure (toCarrier v) }
  | otherwise =
      evalEquationList env rest target point


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Expression evaluator
-- ═══════════════════════════════════════════════════════════════════════

evalExpr
  :: forall sys d a.
     ScalarDesc -> Env -> Expr sys d a -> [Int] -> IO a

evalExpr _desc env (Var pv _ _) point = do
  let varName = symbolVal pv
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

evalExpr desc env (Dep namedM _namedSrc _tok inner) point = do
  let mNM = Named.the namedM
      innerPoint = applyAffineMap (nmNIn mNM) (nmNOut mNM)
                                  (envParams env) (envParamVec env)
                                  (nmConjs mNM) point
  evalExpr desc env inner innerPoint

evalExpr desc env (Reduce reduceOp _namedP namedBody _tok body) point = do
  let bodyNS = Named.the namedBody
      nBody  = nsNDims bodyNS
      n      = length point
      nRed   = nBody - n
      maxVal = envMaxParam env
      redRanges = replicate nRed [0 .. maxVal]
      candidates = sequence redRanges
      validBodyPoints =
        [ point ++ red
        | red <- candidates
        , namedSetMember bodyNS (envParams env) (envParamVec env) (point ++ red)
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

evalExpr desc env (Case _partTok branches) point =
  evalBranches desc env branches point

evalExpr desc env (Restrict _namedSrc _tok inner) point =
  evalExpr desc env inner point


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Branch evaluation
-- ═══════════════════════════════════════════════════════════════════════

evalBranches
  :: forall sys d bs a.
     ScalarDesc -> Env -> CaseBranches sys d bs a -> [Int] -> IO a
evalBranches _ _ BNil point =
  error $ "Alpha.Interpret: no branch matches point " ++ show point
evalBranches desc env (BCons namedB _sub body rest) point =
  if namedSetMember (Named.the namedB) (envParams env) (envParamVec env) point
    then evalExpr desc env body point
    else evalBranches desc env rest point


strictFoldl1 :: (a -> a -> a) -> [a] -> a
strictFoldl1 _ []     = error "Alpha.Interpret: reduction over empty domain"
strictFoldl1 f (x:xs) = foldl' f x xs


-- ═══════════════════════════════════════════════════════════════════════
-- §6. Affine map evaluation (pure Haskell)
-- ═══════════════════════════════════════════════════════════════════════
--
-- Solves the @MapIx@ equality conjunction for the output dims given
-- a concrete input point and parameter assignment.

applyAffineMap
  :: Int -> Int -> Map String Int -> [String]
  -> [Conjunction MapIx] -> [Int] -> [Int]
applyAffineMap ni nOut params paramNames conjs inputPoint =
  let cs = concat [ ks | Conjunction ks <- conjs ]
      paramLookup p
        | p >= 0 && p < length paramNames =
            Map.findWithDefault 0 (paramNames !! p) params
        | otherwise = 0
      dimLookup (InDim d)
        | d < ni    = inputPoint !! d
        | otherwise = 0
      dimLookup (OutDim _) = 0
      dimLookup (MapParam p) = paramLookup p
      -- For each equality constraint, identify the unique OutDim it determines.
      outputMap = Map.fromList
        [ (outIdx, outVal)
        | EqualityConstraint expr <- cs
        , let val0 = evalAffM dimLookup expr
        , outIdx <- [0 .. nOut - 1]
        , let dimLookup1 (OutDim d) = if d == outIdx then 1 else 0
              dimLookup1 ix         = dimLookup ix
              val1  = evalAffM dimLookup1 expr
              coeff = val1 - val0
        , coeff /= 0
        , let outVal = negate val0 `div` coeff
        ]
  in [ Map.findWithDefault 0 j outputMap | j <- [0 .. nOut - 1] ]

evalAffM :: (MapIx -> Int) -> TC.Expr MapIx -> Int
evalAffM ix = go
  where
    go (TC.Ix x)         = ix x
    go (TC.Constant n')  = fromIntegral n'
    go (TC.Mul k e)      = fromIntegral k * go e
    go (TC.Add a b)      = go a + go b
    go (TC.FloorDiv e d) = go e `div` fromIntegral d


-- ═══════════════════════════════════════════════════════════════════════
-- §7. ISL-backed membership testing
-- ═══════════════════════════════════════════════════════════════════════

-- | Does @point@ lie in the named set under @params@?
namedSetMember
  :: NamedSet -> Map String Int -> [String] -> [Int] -> Bool
namedSetMember ns params paramNames point = unsafePerformIO $ runIslT $ Isl.do
  s0 <- materializeNamedSet ns
  s1 <- fixAllParams 0 paramNames params s0
  s2 <- fixAllDims 0 point s1
  Ur empty <- query_ s2 RS.isEmpty
  Isl.pure (Ur (not empty))
{-# NOINLINE namedSetMember #-}

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
-- §8. Parameter context check
-- ═══════════════════════════════════════════════════════════════════════

-- | Re-check that @params@ satisfies the system's stored pctx.
checkPctx :: [String] -> NamedSet -> Map String Int -> Either String ()
checkPctx paramNames pctxNS params = unsafePerformIO $ runIslT $ Isl.do
  s0 <- materializeNamedSet pctxNS
  s1 <- fixAllParams 0 paramNames params s0
  Ur isE <- query_ s1 RS.isEmpty
  if isE
    then Isl.pure (Ur (Left
           ("params = " ++ show (Map.toAscList params))))
    else Isl.pure (Ur (Right ()))
{-# NOINLINE checkPctx #-}
