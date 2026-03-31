{-# LANGUAGE ScopedTypeVariables #-}

-- | Execution engine: interpret 'ValExpr' during scanning.
module Isl.DSL.Execute
  ( -- * Array environment
    ArrayEnv
  , newArrayEnv
  , fillArray
  , readArrayAll
  , readArrayCell
  , writeArrayCell
    -- * Execution
  , execute
  , evalValExpr
  ) where

import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.TypeLits (KnownNat)

import Isl.Scan.Multi (StmtPoint(..), scanMultiForM_)
import Isl.Scan.Types (Vec, mkVec)

import Isl.DSL.Types
import Isl.DSL.Compile


-- ---------------------------------------------------------------------------
-- Array environment
-- ---------------------------------------------------------------------------

-- | Mutable array environment: maps (arrayName, [indices]) -> value.
-- Uses a Map for simplicity (not performance-optimal, but correct).
newtype ArrayEnv = ArrayEnv (IORef (Map String (Map [Integer] Double)))

-- | Create a new empty array environment.
newArrayEnv :: IO ArrayEnv
newArrayEnv = ArrayEnv <$> newIORef Map.empty

-- | Fill an array with values from a function of indices.
-- Requires knowing the concrete bounds.
fillArray :: ArrayEnv -> String -> [Integer] -> [Integer] -> ([Integer] -> Double) -> IO ()
fillArray (ArrayEnv ref) name loBounds hiBounds f = do
  let indices = enumIndices loBounds hiBounds
      cells = Map.fromList [(idx, f idx) | idx <- indices]
  modifyIORef' ref $ Map.insert name cells

-- | Enumerate all index tuples in a rectangular region.
enumIndices :: [Integer] -> [Integer] -> [[Integer]]
enumIndices [] [] = [[]]
enumIndices (lo:los) (hi:his) =
  [ i : rest | i <- [lo..hi], rest <- enumIndices los his ]
enumIndices _ _ = error "enumIndices: mismatched bounds"

-- | Read all cells of an array as a flat list (sorted by index).
readArrayAll :: ArrayEnv -> String -> IO [(([Integer], Double))]
readArrayAll (ArrayEnv ref) name = do
  env <- readIORef ref
  case Map.lookup name env of
    Just cells -> pure (Map.toAscList cells)
    Nothing    -> error $ "readArrayAll: unknown array " ++ show name

-- | Read a single cell.
readArrayCell :: ArrayEnv -> String -> [Integer] -> IO Double
readArrayCell (ArrayEnv ref) name idx = do
  env <- readIORef ref
  case Map.lookup name env >>= Map.lookup idx of
    Just v  -> pure v
    Nothing -> error $ "readArrayCell: " ++ name ++ show idx ++ " not found"

-- | Write a single cell.
writeArrayCell :: ArrayEnv -> String -> [Integer] -> Double -> IO ()
writeArrayCell (ArrayEnv ref) name idx val =
  modifyIORef' ref $ Map.adjust (Map.insert idx val) name


-- ---------------------------------------------------------------------------
-- ValExpr evaluation
-- ---------------------------------------------------------------------------

-- | Evaluate a DSL 'AExpr' given concrete iteration variable and parameter values.
evalAExpr :: Map String Integer  -- ^ variable/param bindings
          -> AExpr -> Integer
evalAExpr env = go
  where
    go (ALit n) = n
    go (AVar name) =
      case Map.lookup name env of
        Just v  -> v
        Nothing -> error $ "evalAExpr: unbound variable " ++ show name
    go (AParam name) =
      case Map.lookup name env of
        Just v  -> v
        Nothing -> error $ "evalAExpr: unbound parameter " ++ show name
    go (AAdd a b) = go a + go b
    go (ANeg a) = negate (go a)
    go (AMul k a) = k * go a
    go (AFloorDiv a k) = go a `div` k
    go (AMod a k) = go a `mod` k

-- | Evaluate a 'ValExpr' given bindings, memory maps, and an array environment.
evalValExpr :: Map String Integer
            -> Map String MemMapDef  -- ^ memory mappings
            -> ArrayEnv
            -> ValExpr -> IO Double
evalValExpr env mmaps arrays = go
  where
    go (VLit r)       = pure (fromRational r)
    go (VLift ae)     = pure (fromIntegral (evalAExpr env ae))
    go (VNeg e)       = negate <$> go e
    go (VBin VAdd a b) = (+) <$> go a <*> go b
    go (VBin VSub a b) = (-) <$> go a <*> go b
    go (VBin VMul a b) = (*) <$> go a <*> go b
    go (VBin VDiv a b) = (/) <$> go a <*> go b
    go (VRead name subs) = do
      let logicalIndices = map (evalAExpr env) subs
          physicalIndices = applyMemMap mmaps name env logicalIndices
      readArrayCell arrays name physicalIndices
    go (VApp name _args) = error $ "evalValExpr: VApp " ++ show name ++ " not implemented"

-- | Apply memory mapping to rewrite logical indices to physical indices.
applyMemMap :: Map String MemMapDef -> String -> Map String Integer -> [Integer] -> [Integer]
applyMemMap mmaps arrayName bindings logicalIndices =
  case Map.lookup arrayName mmaps of
    Nothing -> logicalIndices  -- no mapping, use logical directly
    Just mmap ->
      let ctx = SchedCtx (Map.map ALit bindings)
          logicalExprs = map ALit logicalIndices
          physicalExprs = mmap ctx logicalExprs
          evalEnv = Map.map id bindings
      in map (evalAExpr evalEnv) physicalExprs


-- ---------------------------------------------------------------------------
-- Execution
-- ---------------------------------------------------------------------------

-- | Execute a compiled program with concrete parameter values.
execute
  :: forall nParams. KnownNat nParams
  => Compiled nParams
  -> [(String, Integer)]   -- ^ Parameter bindings: [("N", 8), ("T", 4)]
  -> ArrayEnv              -- ^ Mutable array environment
  -> IO ()
execute compiled paramBindings arrays = do
  let paramMap = Map.fromList paramBindings
      paramVec = mkVec @nParams
                   [fromIntegral v | (_name, v) <- paramBindings]
      stmtMap = cStmts compiled

  let mmaps = cMemMaps compiled

  scanMultiForM_ (cScanner compiled) paramVec $ \pt -> do
    let stmtName = spStmt pt
        origCoords = spOrigCoord pt
    case Map.lookup stmtName stmtMap of
      Nothing -> error $ "execute: unknown statement " ++ show stmtName
      Just es -> do
        -- Build variable bindings: dims + params
        let dimBindings = zip (esDimNames es) origCoords
            allBindings = Map.fromList $
              dimBindings ++ [(n, v) | (n, v) <- paramBindings]
        -- Evaluate RHS (reads go through memory mapping)
        val <- evalValExpr allBindings mmaps arrays (esExpr es)
        -- Evaluate write subscripts and apply memory mapping
        let logicalWriteSubs = map (evalAExpr allBindings) (esSubs es)
            physicalWriteSubs = applyMemMap mmaps (esArrayName es) allBindings logicalWriteSubs
        -- Write to array
        writeArrayCell arrays (esArrayName es) physicalWriteSubs val
