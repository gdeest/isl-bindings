{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

-- | 2D stencil tiling verification with proper separation of concerns.
--
-- Three independent, composable layers:
--   1. DATAFLOW: virtual array A[t,i,j], one statement, pure flow deps
--      (derived automatically from read/write access relations via ISL)
--   2. SCHEDULE: skewing + tiling verified against pure flow deps
--      (always valid — no anti-deps on virtual array)
--   3. MEMORY CONTRACTION: A[t,i,j] → buf[f(t), i, j] verified separately
--      (f is a quasi-affine storage mapping; must not alias live values)
--
-- Design knobs (all in one place, easy to change):
--   - Stencil shape: list of offset vectors
--   - Tile sizes: time tile height, spatial tile widths
--   - Skew factors: per spatial dimension
--   - Buffer count K: for modular contraction t → t mod K
module Main where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO)
import System.IO (hFlush, stdout)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Isl.HighLevel.Context
import qualified Isl.HighLevel.UnionMap as UM
import qualified Isl.HighLevel.UnionSet as US

import qualified Isl.Space as Space
import qualified Isl.Map as RawM

import Isl.AstBuild
import Isl.FlowAnalysis (computeFlowDeps)
import Isl.Infer.Runtime (compileAndLoad, unloadKernel, CompiledKernel(..))

import Foreign.Marshal.Array (mallocArray)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr)
import Foreign.Storable (pokeElemOff, peekElemOff)
import Foreign.C.Types (CLong(..))


-- =========================================================================
-- Design Knobs (change these to explore different configurations)
-- =========================================================================

-- | Stencil offset vectors: each (dt, di, dj) means S[t,i,j] reads A[t+dt, i+di, j+dj].
-- For Jacobi 2D (4-point): reads from 4 spatial neighbors at previous time step.
stencilOffsets :: [(Int, Int, Int)]
stencilOffsets = [(-1,-1,0), (-1,1,0), (-1,0,-1), (-1,0,1)]

-- | Tile sizes.
tileT, tileI, tileJ :: Int
tileT = 4   -- time tile height
tileI = 32  -- spatial tile width (i dimension)
tileJ = 32  -- spatial tile width (j dimension)

-- | Skew factors: skewed coord = original + factor * t
skewI, skewJ :: Int
skewI = 1  -- i' = i + 1*t
skewJ = 1  -- j' = j + 1*t

-- | Number of physical buffers for memory contraction.
-- A[t,i,j] → buf[t mod K][i][j]. Need K > max time reuse distance.
numBuffers :: Int
numBuffers = tileT + 1  -- enough for one tile height


-- =========================================================================
-- Section 1: ISL String Builders (from knobs)
-- =========================================================================

-- Build ISL union map strings from the design knobs.
-- Using ISL strings here is principled: they're the INPUT specification,
-- not an intermediate representation. The knobs above are the source of truth.

-- | Read access relation: { S[t,i,j] → A[t+dt, i+di, j+dj] } for each offset
readAccessStr :: String
readAccessStr =
  "[M, N, T] -> { " ++ commas
    [ "S[t, i, j] -> A[t" ++ off dt ++ ", i" ++ off di ++ ", j" ++ off dj ++ "]"
      ++ " : 1 <= t <= T and 1 <= i <= N and 1 <= j <= M"
    | (dt, di, dj) <- stencilOffsets
    ] ++ " }"
  where off n | n == 0 = "" | n > 0 = " + " ++ show n | otherwise = " - " ++ show (abs n)

-- | Write access relation: { S[t,i,j] → A[t, i, j] }
writeAccessStr :: String
writeAccessStr =
  "[M, N, T] -> { S[t, i, j] -> A[t, i, j] : 1 <= t <= T and 1 <= i <= N and 1 <= j <= M }"

-- | Identity schedule: { S[t,i,j] → [t, i, j] }
identitySchedStr :: String
identitySchedStr =
  "[M, N, T] -> { S[t, i, j] -> [t, i, j] : 1 <= t <= T and 1 <= i <= N and 1 <= j <= M }"

-- | Skewed schedule: { S[t,i,j] → [t, t+i, t+j] }
skewedSchedStr :: String
skewedSchedStr =
  "[M, N, T] -> { S[t, i, j] -> [t, " ++ skewExpr "i" skewI ++ ", " ++ skewExpr "j" skewJ ++ "]"
  ++ " : 1 <= t <= T and 1 <= i <= N and 1 <= j <= M }"
  where skewExpr dim 1 = "t + " ++ dim
        skewExpr dim f = "t + " ++ show f ++ " * " ++ dim

-- | Tiled+skewed schedule with ISL floor division
tiledSchedStr :: String
tiledSchedStr =
  "[M, N, T] -> { S[t, i, j] -> ["
  ++ "floor(t/" ++ show tileT ++ "), "
  ++ "floor((t + i)/" ++ show tileI ++ "), "
  ++ "floor((t + j)/" ++ show tileJ ++ "), "
  ++ "t, t + i, t + j"
  ++ "] : 1 <= t <= T and 1 <= i <= N and 1 <= j <= M }"

-- | Tiled WITHOUT skew (for demonstrating invalid schedule)
tiledNoSkewStr :: String
tiledNoSkewStr =
  "[M, N, T] -> { S[t, i, j] -> ["
  ++ "floor(t/" ++ show tileT ++ "), "
  ++ "floor(i/" ++ show tileI ++ "), "
  ++ "floor(j/" ++ show tileJ ++ "), "
  ++ "t, i, j"
  ++ "] : 1 <= t <= T and 1 <= i <= N and 1 <= j <= M }"

-- | Memory contraction map: A[t,i,j] → buf[t mod K, i, j]
contractionStr :: Int -> String
contractionStr k =
  "[M, N, T] -> { A[t, i, j] -> buf[t mod " ++ show k ++ ", i, j] }"


-- =========================================================================
-- Section 2: Schedule Validity Checker
-- =========================================================================

-- | Build { [a] -> [b] : a >=_lex b } for n dimensions.
buildLexGe :: MonadIO m => Int -> IslT m UM.UnionMap
buildLexGe n = do
  sp <- Space.setAlloc 0 n
  rawMap <- RawM.lexGe sp
  rawUm <- RawM.toUnionMap rawMap
  pure (UM.UnionMap rawUm)

-- | Check schedule validity: deps composed with schedule, check no violations.
checkValidity :: MonadIO m
  => UM.UnionMap   -- ^ dependencies (consumed)
  -> UM.UnionMap   -- ^ schedule (consumed — for applyRange)
  -> UM.UnionMap   -- ^ schedule copy (consumed — for applyDomain)
  -> Int           -- ^ number of schedule output dimensions
  -> IslT m (Ur (Bool, String))
checkValidity deps sched schedCopy nOut = do
  -- { [src] → [θ(dst)] }
  step1 <- UM.applyRange deps sched
  -- { [θ(src)] → [θ(dst)] }
  schedDep <- UM.applyDomain step1 schedCopy
  -- Violations: θ(src) >=_lex θ(dst)
  lexGe <- buildLexGe nOut
  violations <- UM.intersect schedDep lexGe
  (Ur empty, violations') <- UM.isEmpty violations
  (Ur vStr, violations'') <- UM.borrowUM violations' UM.umapToString
  UM.freeUnionMap violations''
  pure (Ur (empty, vStr))


-- =========================================================================
-- Section 3: Memory Contraction Safety Checker
-- =========================================================================

-- | Build contraction-induced anti-deps for modular storage A[t,i,j] → buf[t mod K].
--
-- For each flow dep S[t,i,j] → S[t+1,i',j'] (meaning A[t,i,j] is read at t+1),
-- the next write to buf[t mod K, i, j] is S[t+K, i, j].
-- Anti-dep: S[t+1, i', j'] must execute before S[t+K, i, j].
--
-- Equivalently: for each stencil read offset (dt, di, dj) where dt = -1,
-- S[t, i, j] reads A[t-1, i+di, j+dj], and the next write to
-- buf[(t-1) mod K, i+di, j+dj] is S[t-1+K, i+di, j+dj].
-- Anti-dep: { S[t, i, j] → S[t+K-1, i+di, j+dj] }
contractionAntiDepsStr :: Int -> String
contractionAntiDepsStr k =
  "[M, N, T] -> { " ++ commas
    [ "S[t, i, j] -> S[t" ++ off (k + dt) ++ ", i" ++ off di ++ ", j" ++ off dj ++ "]"
      ++ " : 1 <= t <= T and 1 <= i <= N and 1 <= j <= M"
      ++ " and 1 <= t" ++ off (k + dt) ++ " <= T"
      ++ " and 1 <= i" ++ off di ++ " <= N"
      ++ " and 1 <= j" ++ off dj ++ " <= M"
    | (dt, di, dj) <- stencilOffsets
    ] ++ " }"
  where off n | n == 0 = "" | n > 0 = " + " ++ show n | otherwise = " - " ++ show (abs n)

-- | Check contraction safety under a given schedule.
--
-- Generates contraction-induced anti-deps and validates them against
-- the schedule using the same checkValidity machinery.
-- Safe ⟺ all anti-deps are satisfied by the schedule.
checkContractionSafety :: MonadIO m
  => Int           -- ^ K (number of buffers)
  -> String        -- ^ schedule map string
  -> Int           -- ^ schedule output dimensionality
  -> IslT m (Ur (Bool, String))
checkContractionSafety k schedStr nOut = do
  antiDeps <- UM.fromString (contractionAntiDepsStr k)
  schedA <- UM.fromString schedStr
  schedB <- UM.fromString schedStr
  checkValidity antiDeps schedA schedB nOut


-- =========================================================================
-- Section 4: ISL AST Codegen
-- =========================================================================

-- | Generate C loop skeleton from a schedule via ISL AST builder.
islAstCodegen :: String -> IO String
islAstCodegen schedStr = runIslT $ do
  c <- scheduleMapToC schedStr
  pure (Ur c)

-- | Wrap ISL-generated C in a compilable function with modular buffer addressing.
wrapKernel :: String -> Int -> String -> String
wrapKernel funcName k cSkeleton = unlines
  [ "#include <stdint.h>"
  , "#include <math.h>"
  , ""
  , "// ISL runtime macros"
  , "#define floord(n,d) (((n)<0) ? -((-(n)+(d)-1)/(d)) : (n)/(d))"
  , "#define min(x,y)    (((x)<(y)) ? (x) : (y))"
  , "#define max(x,y)    (((x)>(y)) ? (x) : (y))"
  , ""
  , "// Stencil body with modular buffer addressing"
  , "// buf is an array of " ++ show k ++ " planes, each stride*(N+2) doubles"
  , "#define S(_t_, _i_, _j_) do { \\"
  , "    buf[(((_t_) % " ++ show k ++ ") + " ++ show k ++ ") % " ++ show k ++ "][(_i_) * stride + (_j_)] = \\"
  , "      (buf[((((_t_)-1) % " ++ show k ++ ") + " ++ show k ++ ") % " ++ show k ++ "][((_i_)-1) * stride + (_j_)] \\"
  , "     + buf[((((_t_)-1) % " ++ show k ++ ") + " ++ show k ++ ") % " ++ show k ++ "][((_i_)+1) * stride + (_j_)] \\"
  , "     + buf[((((_t_)-1) % " ++ show k ++ ") + " ++ show k ++ ") % " ++ show k ++ "][(_i_) * stride + ((_j_)-1)] \\"
  , "     + buf[((((_t_)-1) % " ++ show k ++ ") + " ++ show k ++ ") % " ++ show k ++ "][(_i_) * stride + ((_j_)+1)]) * 0.25; \\"
  , "} while(0)"
  , ""
  , "void " ++ funcName ++ "(double** buf, int64_t N, int64_t M, int64_t T, int64_t stride) {"
  ] ++ unlines ["    " ++ l | l <- lines cSkeleton, not (null l)]
  ++ unlines
  [ "}"
  , "#undef floord"
  , "#undef min"
  , "#undef max"
  , "#undef S"
  ]


-- =========================================================================
-- FFI type for compiled kernels
-- =========================================================================

-- buf is double**, N, M, T, stride are int64_t
type StencilKernelC = Ptr (Ptr Double) -> CLong -> CLong -> CLong -> CLong -> IO ()
foreign import ccall "dynamic"
  mkStencilKernel :: FunPtr StencilKernelC -> StencilKernelC


-- =========================================================================
-- Main
-- =========================================================================

main :: IO ()
main = do
  putStrLn "=== 2D Stencil: Dataflow + Tiling + Memory Contraction ==="
  putStrLn ""

  -- ── Design knobs ────────────────────────────────────────────────────────
  putStrLn "── Design Knobs ──"
  putStrLn $ "  Stencil offsets: " ++ show stencilOffsets
  putStrLn $ "  Tile sizes:     t=" ++ show tileT ++ " i=" ++ show tileI ++ " j=" ++ show tileJ
  putStrLn $ "  Skew factors:   i=" ++ show skewI ++ " j=" ++ show skewJ
  putStrLn $ "  Buffers (K):    " ++ show numBuffers
  putStrLn ""

  -- ── 1. Automatic flow dep derivation ──────────────────────────────────
  putStrLn "── 1. Dataflow: automatic dep derivation from access relations ──"
  putStrLn $ "  READ:  " ++ readAccessStr
  putStrLn $ "  WRITE: " ++ writeAccessStr
  hFlush stdout

  flowDeps <- runIslT $ do
    reads <- UM.fromString readAccessStr
    writes <- UM.fromString writeAccessStr
    sched <- UM.fromString identitySchedStr
    let !(UM.UnionMap rawReads) = reads
        !(UM.UnionMap rawWrites) = writes
        !(UM.UnionMap rawSched) = sched
    rawDeps <- computeFlowDeps rawReads rawWrites rawSched
    let deps = UM.UnionMap rawDeps
    (Ur s, deps') <- UM.borrowUM deps UM.umapToString
    UM.freeUnionMap deps'
    pure (Ur s)

  putStrLn $ "  DEPS (auto):  " ++ flowDeps
  putStrLn ""
  hFlush stdout

  -- ── 2. Schedule validity (pure flow deps → tiling works!) ──────────────
  putStrLn "── 2. Schedule validity (pure flow deps, no anti-deps) ──"

  let checkSched label nOut schedStr = do
        (valid, vStr) <- runIslT $ do
          -- Re-derive deps for each check (consumed by checkValidity)
          reads1 <- UM.fromString readAccessStr
          writes1 <- UM.fromString writeAccessStr
          sched1 <- UM.fromString identitySchedStr
          let !(UM.UnionMap r1) = reads1
              !(UM.UnionMap w1) = writes1
              !(UM.UnionMap s1) = sched1
          rawDeps <- computeFlowDeps r1 w1 s1
          let deps = UM.UnionMap rawDeps
          -- Build schedule union maps
          schedA <- UM.fromString schedStr
          schedB <- UM.fromString schedStr
          checkValidity deps schedA schedB nOut
        putStrLn $ "  " ++ label ++ ":"
        putStrLn $ "    " ++ (if valid then "VALID ✓" else "INVALID ✗")
        when (not valid) $
          putStrLn $ "    violations: " ++ take 200 vStr ++ "..."

  checkSched "identity [t, i, j]" 3 identitySchedStr
  checkSched "skewed [t, t+i, t+j]" 3 skewedSchedStr
  checkSched "skewed+tiled" 6 tiledSchedStr
  checkSched "tiled NO skew (EXPECT INVALID)" 6 tiledNoSkewStr
  putStrLn ""
  hFlush stdout

  -- ── 3. Memory contraction verification ─────────────────────────────────
  putStrLn "── 3. Memory contraction: A[t,i,j] → buf[t mod K, i, j] ──"
  putStrLn "  Contraction-induced anti-deps: for each stencil read at t,"
  putStrLn "  the next write to the same buf slot is at t+K-1."
  putStrLn "  These must be satisfied by the schedule."
  putStrLn ""

  let checkContr label k schedStr nOut = do
        (safe, vStr) <- runIslT $ checkContractionSafety k schedStr nOut
        putStrLn $ "  " ++ label ++ ":"
        putStrLn $ "    " ++ (if safe then "SAFE ✓" else "UNSAFE ✗")
        when (not safe) $
          putStrLn $ "    violations: " ++ take 200 vStr ++ "..."

  -- K=2 with different schedules
  checkContr "K=2, identity schedule" 2 identitySchedStr 3
  checkContr "K=2, skewed schedule" 2 skewedSchedStr 3
  checkContr "K=2, tiled+skewed schedule" 2 tiledSchedStr 6
  -- K=tileT+1 with tiled schedule
  checkContr ("K=" ++ show numBuffers ++ ", tiled+skewed schedule") numBuffers tiledSchedStr 6
  -- K=2 with tiled-no-skew (both flow deps AND contraction fail)
  checkContr "K=2, tiled NO skew (EXPECT UNSAFE)" 2 tiledNoSkewStr 6
  putStrLn ""
  hFlush stdout

  -- ── 4. ISL AST codegen ─────────────────────────────────────────────────
  putStrLn "── 4. ISL AST code generation ──"

  putStrLn "  Naive (identity schedule):"
  naiveSkel <- islAstCodegen identitySchedStr
  let naiveSrc = wrapKernel "stencil_naive" 2 naiveSkel
  putStrLn naiveSrc

  putStrLn "  Tiled+skewed schedule:"
  tiledSkel <- islAstCodegen tiledSchedStr
  let tiledSrc = wrapKernel "stencil_tiled" numBuffers tiledSkel
  putStrLn tiledSrc
  hFlush stdout

  -- ── 5. Compile & Benchmark ─────────────────────────────────────────────
  let benchN = 500 :: Int
      benchM = 500 :: Int
      benchT = 50 :: Int
      stride = benchM + 2
      planeSize = (benchN + 2) * (benchM + 2)

  putStrLn $ "── 5. Benchmark: " ++ show benchN ++ "×" ++ show benchM
           ++ " grid, " ++ show benchT ++ " steps ──"

  putStrLn "  Compiling naive kernel..."
  naiveCK <- compileAndLoad "stencil_naive" naiveSrc
  putStrLn "  Compiling tiled kernel..."
  tiledCK <- compileAndLoad "stencil_tiled" tiledSrc
  putStrLn "  Compiled." >> hFlush stdout

  let sampleIdxs = [i * stride + j | i <- [1, benchN `div` 4, benchN `div` 2, benchN]
                                    , j <- [1, benchM `div` 4, benchM `div` 2, benchM]]

  let runKernel ck k = do
        -- Allocate K buffer planes
        planes <- mapM (\_ -> mallocArray planeSize :: IO (Ptr Double)) [1..k]
        let bufArr = planes
        -- Initialize plane 0 (the "t=0" initial state)
        forM_ bufArr $ \p ->
          forM_ [0..planeSize-1] $ \idx -> pokeElemOff p idx (0.0 :: Double)
        -- Set interior of plane 0
        let p0 = head bufArr
        forM_ [1..benchN] $ \i -> forM_ [1..benchM] $ \j ->
          pokeElemOff p0 (i * stride + j) (fromIntegral (i + j) :: Double)
        -- Build buf array (double**)
        bufPtrArr <- mallocArray k :: IO (Ptr (Ptr Double))
        forM_ (zip [0..] bufArr) $ \(idx, p) -> pokeElemOff bufPtrArr idx p
        -- Run
        t0 <- getCurrentTime
        mkStencilKernel (castFunPtr $ ckFuncPtr ck)
          bufPtrArr
          (fromIntegral benchN) (fromIntegral benchM)
          (fromIntegral benchT) (fromIntegral stride)
        t1 <- getCurrentTime
        let elapsed = realToFrac (diffUTCTime t1 t0) :: Double
        -- Read samples from the final plane (t=benchT, plane index = benchT mod k)
        let finalPlane = bufArr !! (benchT `mod` k)
        samples <- mapM (peekElemOff finalPlane) sampleIdxs
        -- Cleanup
        mapM_ free bufArr
        free bufPtrArr
        return (elapsed, samples)

  (naiveTime, naiveRes) <- runKernel naiveCK 2
  (tiledTime, tiledRes) <- runKernel tiledCK numBuffers

  putStrLn $ "  Naive C:   " ++ show (roundTo 4 naiveTime) ++ " s"
  putStrLn $ "  Tiled C:   " ++ show (roundTo 4 tiledTime) ++ " s"
  when (naiveTime > 0) $
    putStrLn $ "  Speedup:   " ++ show (roundTo 2 (naiveTime / tiledTime)) ++ "x"

  let valMismatch = any (\(a, b) -> roundTo 4 a /= roundTo 4 b) (zip naiveRes tiledRes)
  putStrLn $ "  Validation: " ++ if valMismatch then "MISMATCH!" else "OK ✓"

  unloadKernel naiveCK; unloadKernel tiledCK


-- =========================================================================
-- Utilities
-- =========================================================================

roundTo :: Int -> Double -> Double
roundTo n x = fromIntegral (round (x * 10^n) :: Integer) / 10^n

commas :: [String] -> String
commas [] = ""
commas [x] = x
commas (x:xs) = x ++ "; " ++ commas xs
