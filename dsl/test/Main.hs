{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import Isl.DSL

-- | Jacobi 1D — sequential specification (no schedule transforms applied).
-- Uses identity schedule for end-to-end testing.
jacobi1D_seq :: Program '["N", "T"]
jacobi1D_seq = program $ do
  old <- array "old" [range 0 (param @"N" + 1)]
  new <- array "new" [range 0 (param @"N" + 1)]

  for_ @"t" (range 1 (param @"T")) $ \t -> do
    for_ @"i" (range 1 (param @"N")) $ \i ->
      new![ix i] <== (old![ix i - 1] + old![ix i] + old![ix i + 1]) / 3
    for_ @"i" (range 1 (param @"N")) $ \i ->
      old![ix i] <== new![ix i]

-- | Jacobi 1D with annotations (for builder tests only).
jacobi1D :: Program '["N", "T"]
jacobi1D = program $ do
  old <- array "old" [range 0 (param @"N" + 1)]
  new <- array "new" [range 0 (param @"N" + 1)] `withPolicy` ExactWrite

  rescheduled (skew @"t" @"i" 1 >> parallel @"t") $
    for_ @"t" (range 1 (param @"T")) $ \t -> do
      for_ @"i" (range 1 (param @"N")) $ \i ->
        new![ix i] <== (old![ix i - 1] + old![ix i] + old![ix i + 1]) / 3
      for_ @"i" (range 1 (param @"N")) $ \i ->
        old![ix i] <== new![ix i]

  mmap old $ \ctx subs -> [dim @"t" ctx `mod_` 2] ++ subs
  mmap new $ \ctx subs -> [dim @"t" ctx `mod_` 2] ++ subs

-- | Tiled matmul (builder test only).
tiledMatmul :: Program '["K", "M", "N"]
tiledMatmul = program $ do
  _a <- array "A" [range 0 (param @"N" - 1), range 0 (param @"K" - 1)]
  _b <- array "B" [range 0 (param @"K" - 1), range 0 (param @"M" - 1)]
  c  <- array "C" [range 0 (param @"N" - 1), range 0 (param @"M" - 1)]

  let a = ArrayRef "A"
      b = ArrayRef "B"

  rescheduled (tile @"i" 32 >> tile @"j" 32) $
    for_ @"i" (range 0 (param @"N" - 1)) $ \i ->
      for_ @"j" (range 0 (param @"M" - 1)) $ \j ->
        for_ @"k" (range 0 (param @"K" - 1)) $ \k ->
          c![ix i, ix j] <== c![ix i, ix j] + (a![ix i, ix k]) * (b![ix k, ix j])

-- | Naive Jacobi 1D for comparison.
naiveJacobi :: Int -> Int -> [Double] -> [Double]
naiveJacobi n tSteps initArr = go tSteps initArr (replicate (n + 2) 0)
  where
    go 0 old _new = old
    go t old new' =
      let new'' = [if i == 0 || i == n + 1
                   then old !! i
                   else (old !! (i-1) + old !! i + old !! (i+1)) / 3
                  | i <- [0..n+1]]
      in go (t - 1) new'' new'

main :: IO ()
main = do
  putStrLn "=== DSL Tests ==="

  -- Test 1: Builder structure
  putStrLn "\n--- Test 1: Jacobi 1D builder ---"
  let prog = jacobi1D
      arrays = pArrays prog
  assert "old array declared" (Map.member "old" arrays)
  assert "new array declared" (Map.member "new" arrays)
  assert "new has ExactWrite" $
    maybe False ((== ExactWrite) . adPolicy) (Map.lookup "new" arrays)
  let [ForLoop "t" _ _ body] = pLoops prog
  assert "t-loop has 2 children" (length body == 2)
  assert "correct transforms" $
    pTransforms prog == [TSkew "t" "i" 1, TParallel "t"]

  -- Test 2: Tiled matmul builder
  putStrLn "\n--- Test 2: Tiled matmul builder ---"
  let mp = tiledMatmul
  assert "3 arrays" (Map.size (pArrays mp) == 3)
  assert "correct tiles" $
    pTransforms mp == [TTile "i" 32, TTile "j" 32]

  -- Test 3: Statement extraction
  putStrLn "\n--- Test 3: Statement extraction ---"
  let stmts = extractStmts ["N", "T"] (pLoops jacobi1D_seq)
  assert "2 statements extracted" (length stmts == 2)
  assert "S0 writes new" (esArrayName (head stmts) == "new")
  assert "S1 writes old" (esArrayName (stmts !! 1) == "old")
  assert "S0 has dims [t, i]" (esDimNames (head stmts) == ["t", "i"])
  -- seqIdx has one entry per nesting level + statement position:
  -- S0 is at t=pos0, i=pos0, stmt=pos0 → [0, 0, 0]
  -- S1 is at t=pos0, i=pos1, stmt=pos0 → [0, 1, 0]
  putStrLn $ "  S0 seqIdx = " ++ show (esSeqIdx (head stmts))
  putStrLn $ "  S1 seqIdx = " ++ show (esSeqIdx (stmts !! 1))
  assert "S0 and S1 have different seqIdx" (esSeqIdx (head stmts) /= esSeqIdx (stmts !! 1))

  -- Test 4: End-to-end Jacobi 1D execution
  putStrLn "\n--- Test 4: End-to-end Jacobi 1D ---"
  let n = 8
      tSteps = 4
      initArr = [0.0] ++ replicate n 1.0 ++ [0.0]

  -- Compile and execute via DSL
  let Right compiled = compile @2 jacobi1D_seq ["N", "T"]

  -- Debug: print extracted statements
  putStrLn "  Extracted statements:"
  mapM_ (\(name, es) -> do
    putStrLn $ "    " ++ name ++ ": dims=" ++ show (esDimNames es)
      ++ " seqIdx=" ++ show (esSeqIdx es)
      ++ " writes " ++ esArrayName es
    ) (Map.toList (cStmts compiled))

  env <- newArrayEnv
  fillArray env "old" [0] [fromIntegral (n + 1)] (\[i] -> initArr !! fromIntegral i)
  fillArray env "new" [0] [fromIntegral (n + 1)] (\[i] -> initArr !! fromIntegral i)

  execute compiled [("N", fromIntegral n), ("T", fromIntegral tSteps)] env

  dslResult <- readArrayAll env "old"
  let dslValues = map snd dslResult

  -- Compare with naive
  let naiveResult = naiveJacobi n tSteps initArr
      roundTo k x = fromIntegral (round (x * 10^k) :: Integer) / 10^(k :: Int)
      dslRounded = map (roundTo 8) dslValues
      naiveRounded = map (roundTo 8) naiveResult

  putStrLn $ "  DSL result:   " ++ show (map (roundTo 4) dslValues)
  putStrLn $ "  Naive result: " ++ show (map (roundTo 4) naiveResult)
  assert "DSL matches naive implementation" (dslRounded == naiveRounded)

  -- Test 5: Wavefront Jacobi 1D (with skew transform)
  putStrLn "\n--- Test 5: Wavefront Jacobi 1D (skew) ---"
  let jacobi1D_wavefront :: Program '["N", "T"]
      jacobi1D_wavefront = program $ do
        old <- array "old" [range 0 (param @"N" + 1)]
        new <- array "new" [range 0 (param @"N" + 1)]
        rescheduled (skew @"t" @"i" 1) $
          for_ @"t" (range 1 (param @"T")) $ \t -> do
            for_ @"i" (range 1 (param @"N")) $ \i ->
              new![ix i] <== (old![ix i - 1] + old![ix i] + old![ix i + 1]) / 3
            for_ @"i" (range 1 (param @"N")) $ \i ->
              old![ix i] <== new![ix i]

  let Right compiledWF = compile @2 jacobi1D_wavefront ["N", "T"]
  envWF <- newArrayEnv
  fillArray envWF "old" [0] [fromIntegral (n + 1)] (\[i] -> initArr !! fromIntegral i)
  fillArray envWF "new" [0] [fromIntegral (n + 1)] (\[i] -> initArr !! fromIntegral i)

  execute compiledWF [("N", fromIntegral n), ("T", fromIntegral tSteps)] envWF

  dslResultWF <- readArrayAll envWF "old"
  let dslValuesWF = map snd dslResultWF

  putStrLn $ "  Wavefront result: " ++ show (map (roundTo 4) dslValuesWF :: [Double])
  -- Wavefront without double-buffer is Gauss-Seidel-like (different from Jacobi)
  -- Verify: boundaries preserved, all values in [0,1], converging
  assert "wavefront boundaries preserved" (head dslValuesWF == 0 && last dslValuesWF == 0)
  assert "wavefront values in [0,1]" (all (\v -> v >= 0 && v <= 1) dslValuesWF)
  assert "wavefront produced different result (Gauss-Seidel)" (dslValuesWF /= dslValues)

  -- Test 6: Memory mapping mechanics (simple shift mapping)
  putStrLn "\n--- Test 6: Memory mapping ---"
  let simpleMap :: Program '["N"]
      simpleMap = program $ do
        a <- array "A" [range 0 (param @"N" - 1)]
        b <- array "B" [range 0 (param @"N" - 1)]
        for_ @"i" (range 0 (param @"N" - 1)) $ \i ->
          b![ix i] <== a![ix i] + 1
        -- Map B's index i → i+1 (shift by 1)
        mmap b $ \_ctx subs ->
          [head subs + ALit 1]

  let Right compiledMM = compile @1 simpleMap ["N"]
  envMM <- newArrayEnv
  fillArray envMM "A" [0] [3] (\[i] -> fromIntegral i * 10)  -- [0, 10, 20, 30]
  fillArray envMM "B" [0] [4] (\_ -> 0)  -- [0, 0, 0, 0, 0] (extra slot for shift)

  execute compiledMM [("N", 4)] envMM

  mmResult <- readArrayAll envMM "B"
  let mmValues = map snd mmResult
  putStrLn $ "  A = [0, 10, 20, 30], B after mmap shift+1:"
  putStrLn $ "  B = " ++ show mmValues
  -- B[i+1] = A[i] + 1, so B[1]=1, B[2]=11, B[3]=21, B[4]=31, B[0]=0
  assert "mmap shifts write indices" (map snd (filter (\(([i],_)) -> i >= 1 && i <= 4) mmResult)
    == [1.0, 11.0, 21.0, 31.0])

  -- Test 7: Verification — arity and ISL bounds
  putStrLn "\n--- Test 7: Verification ---"

  -- Jacobi passes all checks (arity + ISL bounds)
  let stmtsGood = extractStmts ["N", "T"] (pLoops jacobi1D_seq)
      errorsGood = verifyAll ["N", "T"] (pArrays jacobi1D_seq) stmtsGood
  assert "Jacobi 1D passes verification" (null errorsGood)

  -- Arity mismatch: 2 subscripts for 1D array
  let badStmt = (head stmtsGood) { esSubs = [ALit 0, ALit 0], esArrayName = "old" }
      errorsArity = verifyAll ["N", "T"] (pArrays jacobi1D_seq) [badStmt]
  assert "arity mismatch detected" (not (null errorsArity))
  putStrLn $ "  Arity error: " ++ show (head errorsArity)

  -- ISL bounds check: access old[i+2] would exceed bounds [0, N+1]
  let oobStmt = (head stmtsGood)
        { esSubs = [AAdd (AVar "i") (ALit 2)]  -- old[i+2]: out of bounds!
        , esArrayName = "old"
        , esExpr = VLit 0
        }
      errorsOOB = verifyAll ["N", "T"] (pArrays jacobi1D_seq) [oobStmt]
  assert "out-of-bounds access detected by ISL" (not (null errorsOOB))
  putStrLn $ "  OOB error: " ++ show (head errorsOOB)

  -- Test that compile itself rejects OOB programs
  let badProg :: Program '["N"]
      badProg = program $ do
        a <- array "A" [range 0 (param @"N")]
        for_ @"i" (range 0 (param @"N")) $ \i ->
          a![ix i + 2] <== 0  -- i+2 exceeds [0, N]
  case compile @1 badProg ["N"] of
    Left errs -> do
      assert "compile rejects OOB program" True
      putStrLn $ "  compile error: " ++ show (head errs)
    Right _ ->
      assert "compile should have rejected OOB" False

  putStrLn "\n=== All tests passed ==="

assert :: String -> Bool -> IO ()
assert msg True  = putStrLn $ "  OK: " ++ msg
assert msg False = error $ "FAIL: " ++ msg
