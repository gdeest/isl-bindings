{-# LANGUAGE ForeignFunctionInterface #-}

-- | Kernel correctness test: compare hand-written q8_matvec with
-- the polyhedral-generated version on real model weights.
module Main where

import Data.Int (Int64)
import Data.Word (Word8)
import qualified Data.Map.Strict as Map
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.ForeignPtr (withForeignPtr)
import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Text.Printf (printf)

import Isl.Infer.GGUF
import Isl.Infer.Model
import Isl.Infer.Schedule
import Isl.Infer.Kernel.GEMM
import Isl.Infer.Kernel.Elementwise (q8Matvec, embedToken)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    [path] -> test path
    _      -> putStrLn "Usage: kernel-test <model.gguf>"

test :: FilePath -> IO ()
test path = do
  putStrLn "Loading model..."
  gf <- loadGGUF path
  let cfg = extractConfig gf
      dim = lcDim cfg
      kvd = lcKVDim cfg
      kb  = dim `div` 32

  basePtr <- withForeignPtr (ggufBasePtr gf) return

  let getW name = case Map.lookup name (ggufTensors gf) of
        Just ti -> basePtr `plusPtr` (ggufDataOffset gf + fromIntegral (tiOffset ti))
        Nothing -> error $ "tensor " ++ name ++ " not found"

  -- Create test input: embed token 1 (BOS)
  xBuf <- mallocBytes (dim * 4) :: IO (Ptr Float)
  embedToken xBuf (getW "token_embd.weight") 1 (fromIntegral dim)
  putStrLn "Input: embedded BOS token"

  -- Test Q projection: [dim] -> [dim]
  let wQ = getW "blk.0.attn_q.weight"

  -- Hand-written kernel
  outRef <- mallocBytes (dim * 4) :: IO (Ptr Float)
  q8Matvec outRef xBuf wQ (fromIntegral dim) (fromIntegral dim)
  refVals <- peekArray 10 outRef
  putStrLn $ "Hand-written q8_matvec first 10: " ++ show refVals

  -- Generated kernel (naive schedule)
  putStrLn "Compiling polyhedral naive kernel..."
  cm <- compileMatvec naiveSchedule dim kb
  putStrLn $ "  Source length: " ++ show (length (cmSource cm))

  -- Inspect the generated source briefly
  let srcLines = lines (cmSource cm)
      funcLine = filter (\l -> "void " `isPrefixOf` l) srcLines
  mapM_ (\l -> putStrLn $ "  " ++ l) (take 1 funcLine)

  outGen <- mallocBytes (dim * 4) :: IO (Ptr Float)
  accBuf <- mallocBytes (dim * 4) :: IO (Ptr Float)

  -- Call the generated kernel
  cmFn cm outGen xBuf wQ accBuf (fromIntegral dim) (fromIntegral kb)
  genVals <- peekArray 10 outGen
  putStrLn $ "Generated kernel first 10:       " ++ show genVals

  -- Compare
  refAll <- peekArray dim outRef
  genAll <- peekArray dim outGen
  let maxErr = maximum [abs (a - b) | (a, b) <- zip refAll genAll]
      maxRef = maximum (map abs refAll)
      relErr = if maxRef > 0 then maxErr / maxRef else maxErr
  printf "Max absolute error: %e\n" maxErr
  printf "Max relative error: %e\n" relErr
  printf "Reference max magnitude: %f\n" maxRef

  if relErr < 1e-4
    then putStrLn "PASS: kernels match"
    else putStrLn "FAIL: kernels differ"

  free xBuf; free outRef; free outGen; free accBuf

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _          = True
isPrefixOf _  []         = False
isPrefixOf (a:as) (b:bs) = a == b && isPrefixOf as bs
