{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Jacobi 3D stencil with time-skewed schedule.
--
-- Stencil: new[i][j][k] = (old[i±1][j][k] + old[i][j±1][k] + old[i][j][k±1] + old[i][j][k]) / 7
--
-- Two statements over T time steps, N×M×P interior grid:
--   S0[t,i,j,k]: compute from 7-point stencil
--   S1[t,i,j,k]: copy back
--
-- Skewed schedule (3D wavefront):
--   S0[t,i,j,k] -> [t+i, t+j, t+k, t, 0]
--   S1[t,i,j,k] -> [t+i, t+j, t+k, t, 1]
module Main where

import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.IO (IOUArray)

import Isl.HighLevel.Context
import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Pure
import qualified Isl.HighLevel.UnionSet as US
import qualified Isl.HighLevel.UnionMap as UM
import qualified Isl.Linear as Isl
import Isl.Scan

-- Params: M=0, N=1, P=2, T=3 (alphabetical)

s0Domain :: NamedSet
s0Domain = mkNamedPConjunction @"S0" @'["M","N","P","T"] @4 $
  \(mp :- np :- pp :- tp :- Nil) (t :- i :- j :- k :- Nil) ->
    idx t >=: cst 1 &&: idx t <=: idx tp
    &&: idx i >=: cst 1 &&: idx i <=: idx np
    &&: idx j >=: cst 1 &&: idx j <=: idx mp
    &&: idx k >=: cst 1 &&: idx k <=: idx pp

s1Domain :: NamedSet
s1Domain = mkNamedPConjunction @"S1" @'["M","N","P","T"] @4 $
  \(mp :- np :- pp :- tp :- Nil) (t :- i :- j :- k :- Nil) ->
    idx t >=: cst 1 &&: idx t <=: idx tp
    &&: idx i >=: cst 1 &&: idx i <=: idx np
    &&: idx j >=: cst 1 &&: idx j <=: idx mp
    &&: idx k >=: cst 1 &&: idx k <=: idx pp

-- Skewed schedule: 3D wavefront via t+i, t+j, t+k

s0Sched :: NamedMap
s0Sched = mkNamedPMapConjunction @"S0" @'["M","N","P","T"] @4 @5 $
  \_ (t :- i :- j :- k :- Nil) (w1 :- w2 :- w3 :- tt :- s :- Nil) ->
    idx w1 ==: idx t +: idx i
    &&: idx w2 ==: idx t +: idx j
    &&: idx w3 ==: idx t +: idx k
    &&: idx tt ==: idx t
    &&: idx s ==: cst 0

s1Sched :: NamedMap
s1Sched = mkNamedPMapConjunction @"S1" @'["M","N","P","T"] @4 @5 $
  \_ (t :- i :- j :- k :- Nil) (w1 :- w2 :- w3 :- tt :- s :- Nil) ->
    idx w1 ==: idx t +: idx i
    &&: idx w2 ==: idx t +: idx j
    &&: idx w3 ==: idx t +: idx k
    &&: idx tt ==: idx t
    &&: idx s ==: cst 1

main :: IO ()
main = do
  let nSize = 4   -- interior size in each spatial dim
      mSize = 4
      pSize = 4
      tSteps = 2

  -- Grid (0..N+1)³, boundaries at 0
  old <- newArray ((0,0,0),(nSize+1,mSize+1,pSize+1)) 0.0 :: IO (IOUArray (Int,Int,Int) Double)
  new <- newArray ((0,0,0),(nSize+1,mSize+1,pSize+1)) 0.0 :: IO (IOUArray (Int,Int,Int) Double)
  -- Interior starts at 1.0
  mapM_ (\i -> mapM_ (\j -> mapM_ (\k -> do
    writeArray old (i,j,k) 1.0
    writeArray new (i,j,k) 1.0
    ) [1..pSize]) [1..mSize]) [1..nSize]

  -- Build multi-scanner
  let (nsets, nmaps) = runIsl $ Isl.do
        s0us <- US.toUnionSetFromNamed s0Domain
        s0um <- UM.toUnionMapFromNamed s0Sched
        s0time <- UM.applyToSet s0us s0um
        (Ur ns0, s0t') <- US.decomposeUnionSetNamed s0time
        US.freeUnionSet s0t'

        s1us <- US.toUnionSetFromNamed s1Domain
        s1um <- UM.toUnionMapFromNamed s1Sched
        s1time <- UM.applyToSet s1us s1um
        (Ur ns1, s1t') <- US.decomposeUnionSetNamed s1time
        US.freeUnionSet s1t'

        s0um2 <- UM.toUnionMapFromNamed s0Sched
        s0inv <- UM.reverse s0um2
        (Ur nm0, s0i') <- UM.decomposeUnionMapNamed s0inv
        UM.freeUnionMap s0i'

        s1um2 <- UM.toUnionMapFromNamed s1Sched
        s1inv <- UM.reverse s1um2
        (Ur nm1, s1i') <- UM.decomposeUnionMapNamed s1inv
        UM.freeUnionMap s1i'

        let tag name nsl = [ns { nsName = Just name } | ns <- nsl]
        Isl.pure (Ur (tag "S0" ns0 ++ tag "S1" ns1, nm0 ++ nm1))

  let ms = mkMultiScannerFromNamed @4 nsets nmaps

  putStrLn "=== Jacobi 3D — skewed schedule ==="
  putStrLn $ prettyMultiScanner ["w1", "w2", "w3", "t", "s"] ms

  -- Params: M=0, N=1, P=2, T=3 (alphabetical)
  let params = mkVec @4 [fromIntegral mSize, fromIntegral nSize, fromIntegral pSize, fromIntegral tSteps]

  -- Count points first
  let totalPoints = length (scanMulti ms params)
  putStrLn $ "Total scan points: " ++ show totalPoints

  -- Execute
  scanMultiForM_ ms params $ \pt -> do
    let [t, i, j, k] = map fromIntegral (spOrigCoord pt)
    case spStmt pt of
      "S0" -> do
        xm <- readArray old (i-1, j, k)
        xp <- readArray old (i+1, j, k)
        ym <- readArray old (i, j-1, k)
        yp <- readArray old (i, j+1, k)
        zm <- readArray old (i, j, k-1)
        zp <- readArray old (i, j, k+1)
        c  <- readArray old (i, j, k)
        writeArray new (i, j, k) ((xm + xp + ym + yp + zm + zp + c) / 7.0)
      "S1" -> do
        v <- readArray new (i, j, k)
        writeArray old (i, j, k) v
      _ -> return ()

  -- Print a slice
  putStrLn $ "\nAfter " ++ show tSteps ++ " Jacobi 3D steps on "
          ++ show nSize ++ "×" ++ show mSize ++ "×" ++ show pSize ++ " grid"
  putStrLn "Slice at k=1:"
  mapM_ (\i -> do
    vals <- mapM (\j -> readArray old (i, j, 1)) [0..mSize+1]
    putStrLn $ "  " ++ unwords (map (padLeft 7 . show . roundTo 3) vals)
    ) [0..nSize+1]

  -- Naive verification
  naiveOld <- newArray ((0,0,0),(nSize+1,mSize+1,pSize+1)) 0.0 :: IO (IOUArray (Int,Int,Int) Double)
  naiveNew <- newArray ((0,0,0),(nSize+1,mSize+1,pSize+1)) 0.0 :: IO (IOUArray (Int,Int,Int) Double)
  mapM_ (\i -> mapM_ (\j -> mapM_ (\k -> do
    writeArray naiveOld (i,j,k) 1.0; writeArray naiveNew (i,j,k) 1.0
    ) [1..pSize]) [1..mSize]) [1..nSize]

  mapM_ (\_ -> do
    mapM_ (\i -> mapM_ (\j -> mapM_ (\k -> do
      xm <- readArray naiveOld (i-1,j,k)
      xp <- readArray naiveOld (i+1,j,k)
      ym <- readArray naiveOld (i,j-1,k)
      yp <- readArray naiveOld (i,j+1,k)
      zm <- readArray naiveOld (i,j,k-1)
      zp <- readArray naiveOld (i,j,k+1)
      c  <- readArray naiveOld (i,j,k)
      writeArray naiveNew (i,j,k) ((xm+xp+ym+yp+zm+zp+c)/7.0)
      ) [1..pSize]) [1..mSize]) [1..nSize]
    mapM_ (\i -> mapM_ (\j -> mapM_ (\k -> do
      v <- readArray naiveNew (i,j,k)
      writeArray naiveOld (i,j,k) v
      ) [1..pSize]) [1..mSize]) [1..nSize]
    ) [1..tSteps]

  mismatch <- or <$> sequence
    [ do a <- readArray old (i,j,k); b <- readArray naiveOld (i,j,k); return (roundTo 8 a /= roundTo 8 b)
    | i <- [1..nSize], j <- [1..mSize], k <- [1..pSize]]
  putStrLn $ if mismatch then "MISMATCH!" else "Result matches naive implementation."

  putStrLn $ if scanMulti ms params == scanMultiPQ ms params
    then "Linear and PQ scanners agree."
    else "LINEAR/PQ MISMATCH!"

roundTo :: Int -> Double -> Double
roundTo n x = fromIntegral (round (x * 10^n) :: Integer) / 10^n

padLeft :: Int -> String -> String
padLeft w s = replicate (w - length s) ' ' ++ s
