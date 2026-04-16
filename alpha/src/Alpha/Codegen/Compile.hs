{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Compile, load, and run generated C kernels.
module Alpha.Codegen.Compile
  ( CompiledKernel(..)
  , compileKernel
  , withCompiledKernel
  , runKernel
  , evalBoundStr
  ) where

import Control.Exception (bracket)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as V
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, FunPtr, castFunPtr, castPtr)
import Foreign.Storable (peekElemOff, pokeElemOff, sizeOf)
import System.Exit (ExitCode(..))
import System.Posix.DynamicLinker (DL, dlopen, dlsym, dlclose, RTLDFlags(..))
import System.Process (system)

import Isl.Typed.Params (KnownSymbols, symbolVals)
import Alpha.Core (System, pattern System, Decls(..), DeclList)
import Alpha.Codegen (codegen, CodegenError(..))
import Alpha.Codegen.FunctionMapping
  ( CFunctionMapping(..), ArgPassing(..), declListNames, declListBounds )
import Alpha.Schedule (Schedule)
import Alpha.Allocation (Allocation)


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Types
-- ═══════════════════════════════════════════════════════════════════════

data CompiledKernel = CompiledKernel
  { ckParamNames  :: ![String]
  , ckInputNames  :: ![String]
  , ckOutputNames :: ![String]
  , ckBufNames    :: ![String]            -- all vars, alphabetical (= C arg order)
  , ckBounds      :: !(Map String [String]) -- name → per-dim symbolic bounds
  , ckCall        :: !(FunPtr AlphaCallFn)
  , ckHandle      :: !DL
  }

type AlphaCallFn = Ptr Int64 -> Ptr (Ptr Double) -> IO ()
foreign import ccall "dynamic" mkAlphaCall :: FunPtr AlphaCallFn -> AlphaCallFn


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Compile
-- ═══════════════════════════════════════════════════════════════════════

compileKernel
  :: forall ps inputs outputs locals.
     KnownSymbols ps
  => System ps inputs outputs locals
  -> Schedule -> Allocation -> CFunctionMapping
  -> IO CompiledKernel
compileKernel sys@(System decls _eqs) sched alloc fmap' = do
  let params = symbolVals @ps
      inputNames  = declListNames (dInputs decls)
      outputNames = declListNames (dOutputs decls)
      allBounds   = Map.unions
        [ declListBounds params (dInputs decls)
        , declListBounds params (dOutputs decls)
        , declListBounds params (dLocals decls)
        ]
      bufNames = [ n | (n, CallerAllocated) <- Map.toAscList (cfArgPassing fmap') ]

  result <- codegen sys sched alloc fmap'
  case result of
    Left err -> error $ "compileKernel: codegen failed: " ++ show err
    Right cSrc -> do
      let funcName = cfName fmap'
          cFile  = "/tmp/alpha_ck_" ++ funcName ++ ".c"
          soFile = "/tmp/alpha_ck_" ++ funcName ++ ".so"
      writeFile cFile cSrc
      ec <- system $ "gcc -O2 -shared -fPIC " ++ cFile
                      ++ " -o " ++ soFile ++ " -lm 2>&1"
      case ec of
        ExitSuccess -> pure ()
        _           -> error $ "compileKernel: gcc failed for " ++ cFile
      dl <- dlopen soFile [RTLD_NOW]
      fp <- dlsym dl "alpha_call"
      pure CompiledKernel
        { ckParamNames  = params
        , ckInputNames  = inputNames
        , ckOutputNames = outputNames
        , ckBufNames    = bufNames
        , ckBounds      = allBounds
        , ckCall        = castFunPtr fp
        , ckHandle      = dl
        }

withCompiledKernel
  :: KnownSymbols ps
  => System ps inputs outputs locals
  -> Schedule -> Allocation -> CFunctionMapping
  -> (CompiledKernel -> IO a) -> IO a
withCompiledKernel sys sched alloc fmap' =
  bracket (compileKernel sys sched alloc fmap') (dlclose . ckHandle)


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Run
-- ═══════════════════════════════════════════════════════════════════════

runKernel
  :: CompiledKernel
  -> [Int]                     -- params (alphabetical order)
  -> [V.Vector Double]         -- inputs (declaration order)
  -> IO [V.Vector Double]      -- outputs (declaration order)
runKernel ck paramVals inputVecs = do
  let call     = mkAlphaCall (ckCall ck)
      nParams  = length (ckParamNames ck)
      paramMap = Map.fromList (zip (ckParamNames ck) paramVals)
      bufNames = ckBufNames ck

      -- Compute buffer sizes from symbolic bounds + concrete params
      bufSizes = [ case Map.lookup name (ckBounds ck) of
                     Just bs -> product [ evalBoundStr paramMap b | b <- bs ]
                     Nothing -> 0
                 | name <- bufNames ]

      -- Map input names → vectors (positional)
      inputMap = Map.fromList (zip (ckInputNames ck) inputVecs)

  -- Allocate all buffers
  bufs <- mapM (\sz -> mallocBytes (sz * sizeOf (0 :: Double))) bufSizes
  let bufMap = Map.fromList (zip bufNames bufs)

  -- Fill input buffers, zero output buffers
  sequence_
    [ case Map.lookup name inputMap of
        Just vec -> do
          let ptr = bufMap Map.! name
          V.iforM_ vec $ \i v -> pokeElemOff ptr i v
        Nothing ->
          fillBytes (bufMap Map.! name) 0 (sz * sizeOf (0 :: Double))
    | (name, sz) <- zip bufNames bufSizes ]

  -- Marshal params and buf pointers
  let paramI64s = map fromIntegral paramVals :: [Int64]
      bufPtrs   = map (bufMap Map.!) bufNames
  withInt64Array paramI64s $ \pPtr ->
    withPtrArray bufPtrs $ \bPtr ->
      call pPtr bPtr

  -- Read output buffers
  outputVecs <- mapM (\name -> do
    let ptr = bufMap Map.! name
        sz  = case Map.lookup name (ckBounds ck) of
                Just bs -> product [ evalBoundStr paramMap b | b <- bs ]
                Nothing -> 0
    V.generateM sz (peekElemOff ptr)
    ) (ckOutputNames ck)

  -- Free all buffers
  mapM_ free bufs
  pure outputVecs


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Helpers
-- ═══════════════════════════════════════════════════════════════════════

evalBoundStr :: Map String Int -> String -> Int
evalBoundStr params s = case Map.lookup s params of
  Just v  -> v
  Nothing -> case reads s of
    [(n, "")] -> n
    _         -> error $ "evalBoundStr: can't evaluate '" ++ s ++ "'"

withInt64Array :: [Int64] -> (Ptr Int64 -> IO a) -> IO a
withInt64Array xs action =
  bracket (mallocBytes (length xs * sizeOf (0 :: Int64))) free $ \ptr -> do
    mapM_ (\(i, v) -> pokeElemOff ptr i v) (zip [0..] xs)
    action ptr

withPtrArray :: [Ptr Double] -> (Ptr (Ptr Double) -> IO a) -> IO a
withPtrArray xs action =
  bracket (mallocBytes (length xs * sizeOf (undefined :: Ptr Double))) free $ \ptr -> do
    mapM_ (\(i, v) -> pokeElemOff (castPtr ptr) i v) (zip [0..] xs)
    action (castPtr ptr)
