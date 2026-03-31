{-# LANGUAGE ForeignFunctionInterface #-}

-- | Runtime C compilation and dynamic loading.
--
-- Writes generated C source to a temp file, compiles with GCC
-- (@-O3 -march=native -fopenmp@), and loads the resulting shared
-- library via @dlopen@.
module Isl.Infer.Runtime
  ( CompiledKernel(..)
  , compileAndLoad
  , compileAndLoadWith
  , unloadKernel
  , DynFn
  ) where

import Data.IORef
import Data.Word (Word64)
import Foreign.C.Types (CLong(..))
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray, peekArray)
import Foreign.Ptr (FunPtr, Ptr, castPtr)
import System.Environment (getEnvironment)
import System.FilePath ((</>), (<.>))
import System.Directory (getTemporaryDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (CreateProcess(..), proc, readCreateProcess)
import System.Posix.DynamicLinker
  (DL, dlopen, dlsym, dlclose, RTLDFlags(..))

-- | A compiled and loaded kernel.
data CompiledKernel = CompiledKernel
  { ckDL         :: DL
  , ckFuncPtr    :: FunPtr ()
  , ckSourcePath :: FilePath
  , ckSOPath     :: FilePath
  }

-- | Compile C source to a shared library and load it.
compileAndLoad :: String -> String -> IO CompiledKernel
compileAndLoad = compileAndLoadWith ["-O3", "-march=native", "-fopenmp"]

-- | Like 'compileAndLoad' but with explicit GCC flags.
compileAndLoadWith :: [String] -> String -> String -> IO CompiledKernel
compileAndLoadWith extraFlags funcName cSource = do
  tmpDir <- getTemporaryDirectory
  seqN <- nextSeq
  let tag = funcName ++ "_" ++ show seqN
      srcPath = tmpDir </> ("isl_" ++ tag) <.> "c"
      soPath  = tmpDir </> ("isl_" ++ tag) <.> "so"
  writeFile srcPath cSource
  env <- filter (\(k,_) -> k /= "NIX_ENFORCE_NO_NATIVE") <$> getEnvironment
  let cp = (proc "gcc"
              ( extraFlags ++
              [ "-shared", "-fPIC"
              , "-o", soPath
              , srcPath
              , "-lm"
              ]))
              { env = Just env }
  _ <- readCreateProcess cp ""
  handle <- dlopen soPath [RTLD_NOW]
  fptr <- dlsym handle funcName
  return CompiledKernel
    { ckDL         = handle
    , ckFuncPtr    = fptr
    , ckSourcePath = srcPath
    , ckSOPath     = soPath
    }

-- | Unload a previously loaded kernel.
unloadKernel :: CompiledKernel -> IO ()
unloadKernel ck = dlclose (ckDL ck)

-- | Type alias for dynamic function imports.
type DynFn a = FunPtr a -> a

-- Global counter for unique filenames
{-# NOINLINE seqCounter #-}
seqCounter :: IORef Int
seqCounter = unsafePerformIO (newIORef 0)

nextSeq :: IO Int
nextSeq = atomicModifyIORef' seqCounter (\n -> (n + 1, n))
