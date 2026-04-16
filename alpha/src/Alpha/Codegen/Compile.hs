{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Alpha.Codegen.Compile
  ( CompiledKernel(..)
  , compileKernel
  , withCompiledKernel
  , runKernel
  , evalBoundStr
  , uniformDescs
  ) where

import Control.Exception (bracket)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as V
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, FunPtr, castFunPtr, castPtr)
import Foreign.Storable (sizeOf, pokeElemOff, peekElemOff)
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
import Alpha.Scalar
  ( ScalarDesc(..), AlphaScalar(..), scalarDesc, Marshal(..), cTypeName )


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Types
-- ═══════════════════════════════════════════════════════════════════════

data CompiledKernel = CompiledKernel
  { ckParamNames  :: ![String]
  , ckInputNames  :: ![String]
  , ckOutputNames :: ![String]
  , ckBufNames    :: ![String]
  , ckBounds      :: !(Map String [String])
  , ckDescs       :: !(Map String ScalarDesc)
  , ckCall        :: !(FunPtr AlphaCallFn)
  , ckHandle      :: !DL
  }

-- void** for heterogeneous buffer support
type AlphaCallFn = Ptr Int64 -> Ptr (Ptr ()) -> IO ()
foreign import ccall "dynamic" mkAlphaCall :: FunPtr AlphaCallFn -> AlphaCallFn


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Compile
-- ═══════════════════════════════════════════════════════════════════════

compileKernel
  :: forall ps inputs outputs locals.
     KnownSymbols ps
  => System ps inputs outputs locals
  -> Map String ScalarDesc
  -> Schedule -> Allocation -> CFunctionMapping
  -> IO CompiledKernel
compileKernel sys@(System decls _eqs) descs sched alloc fmap' = do
  let params = symbolVals @ps
      inputNames  = declListNames (dInputs decls)
      outputNames = declListNames (dOutputs decls)
      allBounds   = Map.unions
        [ declListBounds params (dInputs decls)
        , declListBounds params (dOutputs decls)
        , declListBounds params (dLocals decls)
        ]
      bufNames = [ n | (n, CallerAllocated) <- Map.toAscList (cfArgPassing fmap') ]

  result <- codegen sys sched alloc fmap' descs
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
        , ckDescs       = descs
        , ckCall        = castFunPtr fp
        , ckHandle      = dl
        }

-- | Homogeneous convenience: all variables share type @a@.
withCompiledKernel
  :: forall a ps inputs outputs locals r.
     (AlphaScalar a, KnownSymbols ps)
  => System ps inputs outputs locals
  -> Schedule -> Allocation -> CFunctionMapping
  -> (CompiledKernel -> IO r) -> IO r
withCompiledKernel sys sched alloc fmap' =
  let descs = uniformDescs (scalarDesc @a) sys
  in bracket (compileKernel sys descs sched alloc fmap') (dlclose . ckHandle)


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Run
-- ═══════════════════════════════════════════════════════════════════════

-- | Homogeneous convenience: all buffers share type @a@.
runKernel
  :: forall a. (AlphaScalar a, V.Unbox a)
  => CompiledKernel
  -> [Int]
  -> [V.Vector a]
  -> IO [V.Vector a]
runKernel ck paramVals inputVecs = do
  let call     = mkAlphaCall (ckCall ck)
      paramMap = Map.fromList (zip (ckParamNames ck) paramVals)
      bufNames = ckBufNames ck
      m        = scalarMarshal :: Marshal a
        where scalarMarshal = Alpha.Scalar.scalarMarshal @a

      bufSizes = [ case Map.lookup name (ckBounds ck) of
                     Just bs -> product [ evalBoundStr paramMap b | b <- bs ]
                     Nothing -> 0
                 | name <- bufNames ]

      inputMap = Map.fromList (zip (ckInputNames ck) inputVecs)

  -- Allocate all buffers using Marshal's element size
  bufs <- mapM (\sz -> mallocBytes (sz * maElemSize m)) bufSizes
  let bufMap = Map.fromList (zip bufNames bufs)

  -- Fill input buffers, zero output buffers
  sequence_
    [ case Map.lookup name inputMap of
        Just vec -> do
          let ptr = bufMap Map.! name
          V.iforM_ vec $ \i v -> maPoke m (castPtr ptr) i v
        Nothing ->
          fillBytes (bufMap Map.! name) 0 (sz * maElemSize m)
    | (name, sz) <- zip bufNames bufSizes ]

  -- Marshal params and buf pointers
  let paramI64s = map fromIntegral paramVals :: [Int64]
      bufPtrs   = map (\p -> castPtr (bufMap Map.! p)) bufNames
  withInt64Array paramI64s $ \pPtr ->
    withPtrArray bufPtrs $ \bPtr ->
      call pPtr bPtr

  -- Read output buffers
  outputVecs <- mapM (\name -> do
    let ptr = bufMap Map.! name
        sz  = case Map.lookup name (ckBounds ck) of
                Just bs -> product [ evalBoundStr paramMap b | b <- bs ]
                Nothing -> 0
    V.generateM sz $ \i -> maPeek m (castPtr ptr) i
    ) (ckOutputNames ck)

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

withPtrArray :: [Ptr ()] -> (Ptr (Ptr ()) -> IO a) -> IO a
withPtrArray xs action =
  bracket (mallocBytes (length xs * sizeOf (undefined :: Ptr ()))) free $ \ptr -> do
    mapM_ (\(i, v) -> pokeElemOff (castPtr ptr) i v) (zip [0..] xs)
    action (castPtr ptr)

uniformDescs :: forall ps inputs outputs locals.
  ScalarDesc -> System ps inputs outputs locals -> Map String ScalarDesc
uniformDescs desc (System decls _) =
  let names = declListNames (dInputs decls)
           ++ declListNames (dOutputs decls)
           ++ declListNames (dLocals decls)
  in Map.fromList [(n, desc) | n <- names]
