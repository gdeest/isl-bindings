{-# LANGUAGE ForeignFunctionInterface #-}

-- | GEMM codegen benchmark with multi-level tiling via runtime specialization.
--
-- Polyhedra are defined parametrically (for correctness reasoning),
-- then specialized with concrete dimensions before scanning.
-- This makes the scanner instant even for 8+ dimensions.
module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Foreign.C.Types (CLong(..))
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray, peekArray, pokeArray)
import Foreign.Ptr (Ptr, FunPtr, castFunPtr)
import System.CPUTime (getCPUTime)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Text.Printf (printf)

import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Pure (PConjunction, PDisjunction(..), mkPConjunction)
import Isl.Scan (mkScanner, Scanner(..), prettyScanner)
import Isl.Scan.Types (LoopNest(..))

import Isl.Infer.Codegen
import Isl.Infer.Runtime
import Isl.Infer.Specialize

-- ---------------------------------------------------------------------------
-- Timing (CPU time — good enough for single-threaded baselines;
-- for OMP kernels this overmeasures by #threads, but relative
-- comparisons within OMP kernels are still valid)
-- ---------------------------------------------------------------------------

timeIO :: IO a -> IO (a, Double)
timeIO action = do
  start <- getCPUTime
  result <- action
  end <- getCPUTime
  return (result, fromIntegral (end - start) / 1e12)

-- ---------------------------------------------------------------------------
-- Parametric polyhedra (for schedule definition / verification)
-- ---------------------------------------------------------------------------

-- | Truly naive: i,j,k (bad cache, baseline).
-- Params: K=0, M=1, N=2. Dims: i=0, j=1, k=2
ijkDomain :: PConjunction '["K", "M", "N"] 3
ijkDomain = mkPConjunction @'["K","M","N"] @3 $
  \(kp :- mp :- np :- Nil) (i :- j :- k :- Nil) ->
    idx i >=: cst 0 &&: idx i <=: idx np -: cst 1
    &&: idx j >=: cst 0 &&: idx j <=: idx mp -: cst 1
    &&: idx k >=: cst 0 &&: idx k <=: idx kp -: cst 1

-- | Good loop order: i,k,j (cache-friendly for B).
-- Dims: i=0, k=1, j=2
ikjDomain :: PConjunction '["K", "M", "N"] 3
ikjDomain = mkPConjunction @'["K","M","N"] @3 $
  \(kp :- mp :- np :- Nil) (i :- k :- j :- Nil) ->
    idx i >=: cst 0 &&: idx i <=: idx np -: cst 1
    &&: idx k >=: cst 0 &&: idx k <=: idx kp -: cst 1
    &&: idx j >=: cst 0 &&: idx j <=: idx mp -: cst 1

-- | Single-level tiling: ti, tj, i, k, j.
-- Dims: ti=0, tj=1, i=2, k=3, j=4
singleTiledDomain :: Integer -> PConjunction '["K", "M", "N"] 5
singleTiledDomain t = mkPConjunction @'["K","M","N"] @5 $
  \(kp :- mp :- np :- Nil) (ti :- tj :- i :- k :- j :- Nil) ->
        idx ti >=: cst 0 &&: t *: idx ti <=: idx np -: cst 1
    &&: idx tj >=: cst 0 &&: t *: idx tj <=: idx mp -: cst 1
    &&: idx i >=: t *: idx ti &&: idx i <=: t *: idx ti +: cst (t - 1)
    &&: idx i <=: idx np -: cst 1
    &&: idx k >=: cst 0 &&: idx k <=: idx kp -: cst 1
    &&: idx j >=: t *: idx tj &&: idx j <=: t *: idx tj +: cst (t - 1)
    &&: idx j <=: idx mp -: cst 1

-- | Two-level tiling: ti2, tj2, ti1, tj1, tk1, i, k, j.
-- Dims: ti2=0, tj2=1, ti1=2, tj1=3, tk1=4, i=5, k=6, j=7
twoLevelDomain :: Integer -> Integer -> Integer
               -> PConjunction '["K", "M", "N"] 8
twoLevelDomain t2 t1 tk = mkPConjunction @'["K","M","N"] @8 $
  \(kp :- mp :- np :- Nil)
   (ti2 :- tj2 :- ti1 :- tj1 :- tk1 :- i :- k :- j :- Nil) ->
    -- Outer tile
        idx ti2 >=: cst 0 &&: t2 *: idx ti2 <=: idx np -: cst 1
    &&: idx tj2 >=: cst 0 &&: t2 *: idx tj2 <=: idx mp -: cst 1
    -- Inner tile within outer
    &&: t1 *: idx ti1 >=: t2 *: idx ti2
    &&: t1 *: idx ti1 <=: t2 *: idx ti2 +: cst (t2 - 1)
    &&: t1 *: idx ti1 <=: idx np -: cst 1
    &&: t1 *: idx tj1 >=: t2 *: idx tj2
    &&: t1 *: idx tj1 <=: t2 *: idx tj2 +: cst (t2 - 1)
    &&: t1 *: idx tj1 <=: idx mp -: cst 1
    -- K tile
    &&: idx tk1 >=: cst 0 &&: tk *: idx tk1 <=: idx kp -: cst 1
    -- Point loops
    &&: idx i >=: t1 *: idx ti1 &&: idx i <=: t1 *: idx ti1 +: cst (t1 - 1)
    &&: idx i <=: idx np -: cst 1
    &&: idx k >=: tk *: idx tk1 &&: idx k <=: tk *: idx tk1 +: cst (tk - 1)
    &&: idx k <=: idx kp -: cst 1
    &&: idx j >=: t1 *: idx tj1 &&: idx j <=: t1 *: idx tj1 +: cst (t1 - 1)
    &&: idx j <=: idx mp -: cst 1

-- ---------------------------------------------------------------------------
-- Kernel building (specialize + scan + codegen)
-- ---------------------------------------------------------------------------

gemmBody :: String
gemmBody = "C[i * M + j] += A[i * K + k] * B[k * M + j];"

gemmParams :: [(String, String)]
gemmParams =
  [ ("float* restrict", "C")
  , ("const float* restrict", "A")
  , ("const float* restrict", "B")
  , ("int64_t", "K"), ("int64_t", "M"), ("int64_t", "N")
  ]

-- | Build a CKernel from a parametric polyhedron + concrete dimensions.
-- Specializes params, builds scanner, constructs CKernel.
mkGemmCKernel :: String -> [String] -> [String] -> Set Int -> Set Int
              -> PConjunction '["K","M","N"] n
              -> Integer -> Integer -> Integer  -- K, M, N
              -> CKernel
mkGemmCKernel name dimNames paramNames parDims simdDims domain kVal mVal nVal =
  let specialized = specialize [kVal, mVal, nVal] domain
      Scanner nests = mkScanner (PDisjunction [specialized])
  in CKernel
    { ckName = name, ckIncludes = [], ckTypedefs = "", ckMacros = []
    , ckReturnType = "void", ckFuncParams = gemmParams
    , ckDimNames = dimNames, ckParamNames = paramNames
    , ckParallelDims = parDims, ckSimdDims = simdDims
    , ckPreamble = "", ckBody = gemmBody, ckPostamble = ""
    , ckLoopNests = map eraseLoopNest nests
    }

-- ---------------------------------------------------------------------------
-- FFI
-- ---------------------------------------------------------------------------

type GemmFn = Ptr Float -> Ptr Float -> Ptr Float -> CLong -> CLong -> CLong -> IO ()

foreign import ccall "dynamic"
  mkGemmFn :: FunPtr GemmFn -> GemmFn

fi :: Int -> CLong
fi = fromIntegral

compileFn :: String -> String -> IO GemmFn
compileFn name src = do
  k <- compileAndLoad name src
  return (mkGemmFn (castFunPtr (ckFuncPtr k)))

compileFnWith :: [String] -> String -> String -> IO GemmFn
compileFnWith flags name src = do
  k <- compileAndLoadWith flags name src
  return (mkGemmFn (castFunPtr (ckFuncPtr k)))

-- ---------------------------------------------------------------------------
-- Benchmark helpers
-- ---------------------------------------------------------------------------

benchKernel :: GemmFn -> Ptr Float -> Ptr Float -> Ptr Float
            -> Int -> Int -> Int -> Int -> IO Double
benchKernel fn aPtr bPtr cPtr n m k nIters = do
  pokeArray cPtr (replicate (n * m) (0 :: Float))
  fn cPtr aPtr bPtr (fi k) (fi m) (fi n)  -- warm up
  (_, t) <- timeIO $ sequence_ $ replicate nIters $ do
    pokeArray cPtr (replicate (n * m) (0 :: Float))
    fn cPtr aPtr bPtr (fi k) (fi m) (fi n)
  return (t / fromIntegral nIters)

verifyGemm :: GemmFn -> Int -> Int -> Int -> IO ()
verifyGemm fn n m k = do
  aPtr <- mallocArray (n * k) :: IO (Ptr Float)
  bPtr <- mallocArray (k * m)
  cPtr <- mallocArray (n * m)
  let aVals = [fromIntegral ((i * 3 + 7) `mod` 13) :: Float | i <- [0..n*k-1]]
      bVals = [fromIntegral ((i * 5 + 3) `mod` 11) :: Float | i <- [0..k*m-1]]
  pokeArray aPtr aVals; pokeArray bPtr bVals
  pokeArray cPtr (replicate (n * m) (0 :: Float))
  fn cPtr aPtr bPtr (fi k) (fi m) (fi n)
  result <- peekArray (n * m) cPtr
  free aPtr; free bPtr; free cPtr
  let expected = [ sum [ (aVals !! (i' * k + kk)) * (bVals !! (kk * m + j'))
                       | kk <- [0..k-1] ]
                 | i' <- [0..n-1], j' <- [0..m-1] ]
      maxErr = maximum [abs (a - b) | (a, b) <- zip result expected]
  if maxErr < 1.0 then pure ()
    else error $ "MISMATCH: max error = " ++ show maxErr

gf :: Int -> Double -> Double
gf n t = 2.0 * fromIntegral n ^ (3 :: Int) / t / 1e9

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "step 1: build ikj domain"
  let Scanner [n1] = mkScanner (PDisjunction [specialize [256,256,256] ikjDomain])
  putStrLn $ "  levels: " ++ show (length (lnLevels n1))
  putStrLn "step 2: build single-tiled domain"
  let Scanner [n2] = mkScanner (PDisjunction [specialize [256,256,256] (singleTiledDomain 64)])
  putStrLn $ "  levels: " ++ show (length (lnLevels n2))
  putStrLn "step 3: build two-level domain"
  let Scanner [n3] = mkScanner (PDisjunction [specialize [256,256,256] (twoLevelDomain 128 32 64)])
  putStrLn $ "  levels: " ++ show (length (lnLevels n3))
  putStrLn "step 4: generate C"
  let ck = mkGemmCKernel "test" ["ti2","tj2","ti1","tj1","tk1","i","k","j"] ["K","M","N"]
             (Set.fromList [0,1]) (Set.singleton 7) (twoLevelDomain 128 32 64) 256 256 256
  putStrLn (generateC ck)
  putStrLn "step 5: compile + run"
  fn <- compileFn "test" (generateC ck)
  verifyGemm fn 256 256 256
  putStrLn "  correctness: OK"
  putStrLn "done"

_main_real :: IO ()
_main_real = do
  putStrLn "=== ISL-Infer GEMM Benchmark ==="
  putStrLn "=== Runtime specialization + multi-level tiling ==="
  putStrLn ""

  let sz = 2048
      szI = fromIntegral sz

  -- Allocate once
  aPtr <- mallocArray (sz * sz) :: IO (Ptr Float)
  bPtr <- mallocArray (sz * sz)
  cPtr <- mallocArray (sz * sz)
  pokeArray aPtr [fromIntegral (i `mod` 7) :: Float | i <- [0..sz*sz-1]]
  pokeArray bPtr [fromIntegral (i `mod` 11) :: Float | i <- [0..sz*sz-1]]

  -- Baseline: -O3 single-thread ikj
  putStrLn "--- Baseline ---"
  let ikjK  = mkGemmCKernel "gikj" ["i","k","j"] ["K","M","N"]
                Set.empty Set.empty ikjDomain szI szI szI

  ikjFn  <- compileFn "gikj" (generateC ikjK)
  verifyGemm ikjFn 64 64 64

  tO3 <- benchKernel ikjFn aPtr bPtr cPtr sz sz sz 3
  printf "  -O3 ikj:     %8.1f ms  %6.1f GFLOPS  (single-thread baseline)\n"
    (tO3*1000::Double) (gf sz tO3)

  -- Single-level tiling sweep
  putStrLn ""
  putStrLn "--- Single-level tiling (specialized, parallel) ---"
  printf "  %-8s %10s %10s %10s\n"
    ("Tile" :: String) ("ms" :: String) ("GFLOPS" :: String) ("vs -O3" :: String)

  mapM_ (\t -> do
    let ck = mkGemmCKernel "gs1" ["ti","tj","i","k","j"] ["K","M","N"]
               (Set.fromList [0,1]) (Set.singleton 4)
               (singleTiledDomain t) szI szI szI
    fn <- compileFn "gs1" (generateC ck)
    verifyGemm fn 128 128 128
    tK <- benchKernel fn aPtr bPtr cPtr sz sz sz 3
    printf "  T=%-5d %10.1f %10.1f %9.1fx\n"
      t (tK*1000::Double) (gf sz tK) (tO3/tK::Double)
    ) [32, 48, 64, 96, 128, 192, 256]

  -- Two-level tiling sweep (the key test — was hanging before)
  putStrLn ""
  putStrLn "--- Two-level tiling (specialized, parallel) ---"
  putStrLn "--- THIS used to hang with parametric scanner ---"
  printf "  %-18s %10s %10s %10s\n"
    ("T2/T1/TK" :: String) ("ms" :: String) ("GFLOPS" :: String) ("vs -O3" :: String)

  mapM_ (\(t2, t1, tk) -> do
    let ck = mkGemmCKernel "g2l"
               ["ti2","tj2","ti1","tj1","tk1","i","k","j"]
               ["K","M","N"]
               (Set.fromList [0,1])  -- parallel outer tiles
               (Set.singleton 7)    -- SIMD on j
               (twoLevelDomain t2 t1 tk) szI szI szI
    fn <- compileFn "g2l" (generateC ck)
    verifyGemm fn 256 256 256
    tK <- benchKernel fn aPtr bPtr cPtr sz sz sz 3
    printf "  %3d/%3d/%-8d %10.1f %10.1f %9.1fx\n"
      t2 t1 tk (tK*1000::Double) (gf sz tK) (tO3/tK::Double)
    ) [ (128, 16, 64), (128, 32, 64), (128, 32, 128), (128, 48, 128)
      , (192, 32, 64), (192, 32, 128), (192, 48, 128)
      , (256, 32, 128), (256, 48, 128), (256, 64, 128)
      , (384, 32, 128), (384, 48, 128)
      , (512, 32, 128), (512, 48, 128)
      ]

  -- Size scaling with best config
  putStrLn ""
  putStrLn "--- Size scaling ---"
  printf "  %-10s %12s %16s  %7s  %10s\n"
    ("Size" :: String) ("-O3 ikj" :: String)
    ("2-lvl+OMP" :: String) ("vs -O3" :: String) ("GFLOPS" :: String)

  free aPtr; free bPtr; free cPtr

  mapM_ (\s -> do
    let sI = fromIntegral s
    a <- mallocArray (s * s) :: IO (Ptr Float)
    b <- mallocArray (s * s)
    c <- mallocArray (s * s)
    pokeArray a [fromIntegral (i `mod` 7) :: Float | i <- [0..s*s-1]]
    pokeArray b [fromIntegral (i `mod` 11) :: Float | i <- [0..s*s-1]]

    -- Specialize for this exact size
    let ikj = mkGemmCKernel "gikj" ["i","k","j"] ["K","M","N"]
                Set.empty Set.empty ikjDomain sI sI sI
        best = mkGemmCKernel "gbst"
                 ["ti2","tj2","ti1","tj1","tk1","i","k","j"] ["K","M","N"]
                 (Set.fromList [0,1]) (Set.singleton 7)
                 (twoLevelDomain 256 32 128) sI sI sI

    fnO3 <- compileFn "gikj" (generateC ikj)
    fnB  <- compileFn "gbst" (generateC best)

    let iN = max 1 (min 5 (div (256*256*256) (s*s*s)))
        iT = max 1 (min 5 (div (512*512*512) (s*s*s)))

    t3 <- benchKernel fnO3 a b c s s s iN
    tB <- benchKernel fnB a b c s s s iT
    free a; free b; free c

    printf "  %4d×%-4d  %10.1f   %14.1f  %6.1fx  %8.1f\n"
      s s (t3*1000::Double) (tB*1000::Double)
      (t3/tB::Double) (gf s tB::Double)
    ) [256, 512, 1024, 2048]

  putStrLn "\nDone."
