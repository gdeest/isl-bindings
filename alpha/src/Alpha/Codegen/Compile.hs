{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Alpha.Codegen.Compile
  ( CompiledKernel(..)
  , CompileException(..)
  , compileKernel
  , withCompiledKernel
  , runKernel
  , executeKernelHet
  , evalBoundStr
  , uniformDescs
  ) where

import Control.Exception (Exception, bracket, finally, throwIO)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as V
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Marshal.Utils (fillBytes, copyBytes)
import Foreign.Ptr (Ptr, FunPtr, castFunPtr, castPtr)
import Foreign.Storable (sizeOf, pokeElemOff, peekElemOff)
import System.Exit (ExitCode(..))
import System.Posix.DynamicLinker (DL, dlopen, dlsym, dlclose, RTLDFlags(..))
import System.Process (system)

import GHC.TypeLits (KnownNat)
import Isl.Typed.Params (KnownSymbols, Length, symbolVals)
import Isl.Monad (Ur(..), runIslT)
import qualified Isl.Linear as Isl
import Alpha.Surface.Core (System, pattern System, Decls(..))
import Alpha.Codegen (codegen, CodegenError)
import Alpha.Codegen.FunctionMapping
  ( CFunctionMapping(..), ArgPassing(..), declListNames, declListBoundsM )
import Alpha.Compile (compile, CompileError)
import Alpha.Schedule (Schedule)
import Alpha.Allocation (Allocation)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Alpha.Scalar
  ( ScalarDesc(..), AlphaScalar(..), scalarDesc, Marshal(..), cTypeName
  , SomeBuffer(..) )


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

-- | Structured failure modes of 'compileKernel'.  Thrown instead of
-- 'error' so callers can 'try' on a typed exception rather than
-- pattern-matching on a stringly-typed 'ErrorCall'.
data CompileException
  = CompileBoundExtractionFailed !String !Int !String
    -- ^ @declListBoundsM@ failed for @(varName, dim, reason)@.
  | CompileValidationFailed !CompileError
    -- ^ 'Alpha.Compile.compile' rejected the (system, schedule,
    -- allocation) triple — schedule violation, post-contraction WAW,
    -- unsound annotation, etc.
  | CompileCodegenFailed !CodegenError
  | CompileGccFailed !String
    -- ^ @gcc -O2 -shared -fPIC@ exited non-zero on the named @.c@ file.
  deriving (Show)

instance Exception CompileException


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Compile
-- ═══════════════════════════════════════════════════════════════════════

compileKernel
  :: forall ps pctx inputs outputs locals.
     (KnownSymbols ps, KnownNat (Length ps))
  => System ps pctx inputs outputs locals
  -> Map String ScalarDesc
  -> Schedule -> Allocation -> CFunctionMapping
  -> IO CompiledKernel
compileKernel sys@(System decls _eqs) descs sched alloc fmap' = do
  let params = symbolVals @ps
      inputNames  = declListNames (dInputs decls)
      outputNames = declListNames (dOutputs decls)
      bufNames = [ n | (n, CallerAllocated) <- Map.toAscList (cfArgPassing fmap') ]

  allBoundsE <- runIslT $ Isl.do
    Ur i <- declListBoundsM params (dInputs decls)
    Ur o <- declListBoundsM params (dOutputs decls)
    Ur l <- declListBoundsM params (dLocals decls)
    Isl.pure (Ur (do
      mi <- i; mo <- o; ml <- l
      Right (Map.unions [mi, mo, ml])))
  allBounds <- case allBoundsE of
    Right m -> pure m
    Left (n, d, err) ->
      throwIO (CompileBoundExtractionFailed n d (show err))

  -- Validate first; codegen takes the typed 'Compiled' artifact as
  -- proof that the system passed all polyhedral checks (schedule
  -- positivity, post-contraction WAW, parallel/vectorize annotation
  -- soundness).
  compiled <- compile sys sched alloc >>= \case
    Left err -> throwIO (CompileValidationFailed err)
    Right c  -> pure c
  result <- codegen compiled fmap' descs
  case result of
    Left err -> throwIO (CompileCodegenFailed err)
    Right cSrc -> do
      let funcName = cfName fmap'
          cFile  = "/tmp/alpha_ck_" ++ funcName ++ ".c"
          soFile = "/tmp/alpha_ck_" ++ funcName ++ ".so"
      writeFile cFile cSrc
      ec <- system $ "gcc -O2 -shared -fPIC " ++ cFile
                      ++ " -o " ++ soFile ++ " -lm 2>&1"
      case ec of
        ExitSuccess -> pure ()
        _           -> throwIO (CompileGccFailed cFile)
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
  :: forall a ps pctx inputs outputs locals r.
     (AlphaScalar a, KnownSymbols ps, KnownNat (Length ps))
  => System ps pctx inputs outputs locals
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

  bufs <- mapM (\sz -> mallocBytes (sz * maElemSize m)) bufSizes
  flip finally (mapM_ free bufs) $ do
    let bufMap = Map.fromList (zip bufNames bufs)

    sequence_
      [ case Map.lookup name inputMap of
          Just vec -> do
            let ptr = bufMap Map.! name
            V.iforM_ vec $ \i v -> maPoke m (castPtr ptr) i v
          Nothing ->
            fillBytes (bufMap Map.! name) 0 (sz * maElemSize m)
      | (name, sz) <- zip bufNames bufSizes ]

    let paramI64s = map fromIntegral paramVals :: [Int64]
        bufPtrs   = map (\p -> castPtr (bufMap Map.! p)) bufNames
    withInt64Array paramI64s $ \pPtr ->
      withPtrArray bufPtrs $ \bPtr ->
        call pPtr bPtr

    mapM (\name -> do
      let ptr = bufMap Map.! name
          sz  = case Map.lookup name (ckBounds ck) of
                  Just bs -> product [ evalBoundStr paramMap b | b <- bs ]
                  Nothing -> 0
      V.generateM sz $ \i -> maPeek m (castPtr ptr) i
      ) (ckOutputNames ck)


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Heterogeneous execution
-- ═══════════════════════════════════════════════════════════════════════

-- | Execute a compiled kernel with per-variable typed buffers.
-- Input SomeBuffers are matched to ckInputNames by position.
-- Returns output SomeBuffers in ckOutputNames order.
executeKernelHet
  :: CompiledKernel -> [Int] -> [SomeBuffer] -> IO [SomeBuffer]
executeKernelHet ck paramVals inputBufs = do
  let call     = mkAlphaCall (ckCall ck)
      paramMap = Map.fromList (zip (ckParamNames ck) paramVals)
      bufNames = ckBufNames ck
      inputMap = Map.fromList (zip (ckInputNames ck) inputBufs)

      bufSize name = case Map.lookup name (ckBounds ck) of
        Just bs -> product [ evalBoundStr paramMap b | b <- bs ]
        Nothing -> 0

      lookupDesc name = case Map.lookup name (ckDescs ck) of
        Just d  -> d
        Nothing -> error $ "executeKernelHet: no ScalarDesc for " ++ name

      elemSz name = case lookupDesc name of
        MkScalarDesc { sdMarshal = Just m } -> maElemSize m
        _ -> error $ "executeKernelHet: no Marshal for " ++ name

  -- Bracket raw buffer allocation to prevent leaks on exception
  rawBufs <- mapM (\name -> mallocBytes (bufSize name * elemSz name)) bufNames
  flip finally (mapM_ free rawBufs) $ do
    let rawBufMap = Map.fromList (zip bufNames rawBufs)

    -- Copy input data; zero non-input buffers
    sequence_
      [ case Map.lookup name inputMap of
          Just sb -> withForeignPtr (sbData sb) $ \srcPtr -> do
            let dst = rawBufMap Map.! name
                allocBytes = bufSize name * elemSz name
                copyLen = min allocBytes (sbElemCount sb * elemSz name)
            copyBytes dst (castPtr srcPtr) copyLen
          Nothing ->
            fillBytes (rawBufMap Map.! name) 0 (bufSize name * elemSz name)
      | name <- bufNames ]

    -- Call the kernel
    let paramI64s = map fromIntegral paramVals :: [Int64]
        bufPtrs = map (\n -> castPtr (rawBufMap Map.! n)) bufNames
    withInt64Array paramI64s $ \pPtr ->
      withPtrArray bufPtrs $ \bPtr ->
        call pPtr bPtr

    -- Read output buffers into SomeBuffers
    mapM (\name -> do
      let sz = bufSize name
          esz = elemSz name
      fptr <- mallocForeignPtrBytes (sz * esz)
      withForeignPtr fptr $ \dst ->
        copyBytes (castPtr dst) (rawBufMap Map.! name) (sz * esz)
      pure SomeBuffer
        { sbDesc = lookupDesc name
        , sbData = fptr
        , sbElemCount = sz
        }
      ) (ckOutputNames ck)


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Helpers
-- ═══════════════════════════════════════════════════════════════════════

-- | Evaluate a symbolic bound expression against concrete parameter values.
-- Handles: integers, parameter names, +, -, *, parentheses, floord(n,d).
evalBoundStr :: Map String Int -> String -> Int
evalBoundStr params = fst . parseAddSub . filter (/= ' ')
  where
    -- additive: expr (+|-) term
    parseAddSub :: String -> (Int, String)
    parseAddSub s = let (a, rest) = parseMulDiv s in goAdd a rest
    goAdd lhs ('+':rest) = let (rhs, rest') = parseMulDiv rest in goAdd (lhs + rhs) rest'
    goAdd lhs ('-':rest) = let (rhs, rest') = parseMulDiv rest in goAdd (lhs - rhs) rest'
    goAdd lhs rest = (lhs, rest)

    -- multiplicative: atom (*|/) atom
    parseMulDiv :: String -> (Int, String)
    parseMulDiv s = let (a, rest) = parseAtom s in goMul a rest
    goMul lhs ('*':rest) = let (rhs, rest') = parseAtom rest in goMul (lhs * rhs) rest'
    goMul lhs ('/':rest) = let (rhs, rest') = parseAtom rest in goMul (lhs `div` rhs) rest'
    goMul lhs rest = (lhs, rest)

    parseAtom :: String -> (Int, String)
    parseAtom ('-':rest) = let (v, rest') = parseAtom rest in (negate v, rest')
    parseAtom ('(':rest) =
      let (v, rest') = parseAddSub rest
      in case rest' of
        ')':rest'' -> (v, rest'')
        _          -> error $ "evalBoundStr: unmatched paren"
    parseAtom ('f':'l':'o':'o':'r':'d':'(':rest) =
      case parseAddSub rest of
        (n, ',':rest') -> case parseAddSub rest' of
          (d, ')':rest'') -> (if n < 0 then -((-n + d - 1) `div` d) else n `div` d, rest'')
          _ -> error "evalBoundStr: malformed floord"
        _ -> error "evalBoundStr: malformed floord"
    parseAtom s = case span isDigit s of
      (ds@(_:_), rest) ->
        let n = read ds
        in case rest of
          -- Juxtaposition: "2N" ≡ "2 * N" (ISL emits coefficient·param
          -- products this way).  Binds tighter than '*' / '/' — keyed
          -- off isIdent, and rest can't start with a digit since span
          -- isDigit already consumed them.
          (c:_) | isIdent c ->
            let (v, rest') = parseAtom rest in (n * v, rest')
          _ -> (n, rest)
      _ -> case break (not . isIdent) s of
        (name@(_:_), rest) -> case Map.lookup name params of
          Just v  -> (v, rest)
          Nothing -> error $ "evalBoundStr: unknown param '" ++ name ++ "'"
        _ -> error $ "evalBoundStr: can't parse '" ++ s ++ "'"

    isDigit c = c >= '0' && c <= '9'
    isIdent c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || isDigit c

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

uniformDescs :: forall ps pctx inputs outputs locals.
  ScalarDesc -> System ps pctx inputs outputs locals -> Map String ScalarDesc
uniformDescs desc (System decls _) =
  let names = declListNames (dInputs decls)
           ++ declListNames (dOutputs decls)
           ++ declListNames (dLocals decls)
  in Map.fromList [(n, desc) | n <- names]
