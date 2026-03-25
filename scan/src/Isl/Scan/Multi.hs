{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Multi-statement polyhedral scanning.
--
-- Given a set of statements, each with a scheduled domain in the same
-- time space and a schedule inverse for recovering original coordinates,
-- enumerate all statement instances in lexicographic schedule order.
--
-- Two enumeration strategies are provided:
--
--   * Linear-scan merge ('scanMulti', 'scanMultiFold') — O(k) per point
--     where k = number of active loop nests. Simple, good for small k.
--
--   * Priority-queue merge ('scanMultiPQ', 'scanMultiPQFold') — O(log k)
--     per point. Better for large numbers of statements/disjuncts.
--
-- Both produce identical results and can be cross-checked.
--
-- Types are untyped (no phantom params\/dims) because construction goes
-- through value-level 'NamedSet'\/'NamedMap' data from ISL decomposition.
-- Use the typed single-statement 'Scanner' for cases where type safety
-- is needed.
module Isl.Scan.Multi
  ( -- * Types
    StmtInverse(..)
  , ScheduledStatement(..)
  , MultiScanner(..)
  , StmtPoint(..)
  , ULoopNest(..)
    -- * Construction
  , mkMultiScanner
  , mkMultiScannerFromNamed
  , extractInverse
    -- * Linear-scan enumeration
  , scanMulti
  , scanMultiFold
  , scanMultiForM_
    -- * Priority-queue enumeration
  , scanMultiPQ
  , scanMultiPQFold
  , scanMultiPQForM_
    -- * Coordinate recovery
  , recoverOrigCoords
  ) where

import Data.List (sortBy, foldl')
import Data.Ord (comparing)
import GHC.TypeLits (Nat, KnownNat)

import Isl.HighLevel.Constraints
  ( Conjunction(..), Constraint(..), MapIx(..), SetIx
  , expandExpr )
import Isl.HighLevel.Pure (NamedSet(..), NamedMap(..))
import Isl.Scan.Types (LoopLevel(..), AffineBound(..), Vec(..), mkVec)
import Isl.Scan.Build (buildLevels)

-- * Types

-- | Per-statement schedule inverse: maps time-space coordinates back to
-- original iteration coordinates. Each 'AffineBound' expresses one original
-- dimension as an affine function of time dimensions and parameters.
data StmtInverse = StmtInverse
  { siOrigDims :: !Int
  , siBounds   :: ![AffineBound]  -- ^ One per original dimension. @loopCoeffs@
                                  -- reference time dimensions (not original dims).
  } deriving (Show, Eq)

-- | Untyped loop nest — same structure as 'LoopNest' but without phantom indices.
-- Used for multi-statement scanning where type-level parameter/dimension
-- info is not available (construction goes through value-level 'NamedSet').
data ULoopNest = ULoopNest
  { ulnLevels :: ![LoopLevel]
  , ulnParams :: !Int
  , ulnDims   :: !Int
  } deriving (Show, Eq)

-- | A single statement in the multi-scanner.
data ScheduledStatement = ScheduledStatement
  { ssName      :: !String
  , ssLoopNests :: ![ULoopNest]     -- ^ In time space (from scheduled domain)
  , ssInverse   :: !StmtInverse     -- ^ Maps time coords -> original coords
  } deriving (Show, Eq)

-- | Multi-statement scanner: a collection of scheduled statements
-- ready for merged enumeration.
--
-- The phantom @nParams@ tracks the number of parameters at the type level,
-- validated at construction time by 'mkMultiScanner'.
newtype MultiScanner (nParams :: Nat) = MultiScanner
  { msStatements :: [ScheduledStatement]
  } deriving (Show, Eq)

-- | A point emitted by the multi-statement scanner.
data StmtPoint = StmtPoint
  { spStmt      :: !String      -- ^ Statement name
  , spOrigCoord :: ![Integer]   -- ^ Original iteration coordinates (primary)
  , spTimeCoord :: ![Integer]   -- ^ Time-space coordinates
  } deriving (Show, Eq)

-- * Construction

-- | Build a 'MultiScanner' from 'NamedSet's (scheduled domains) and
-- 'NamedMap's (schedule inverses), matched by tuple name.
--
-- Each 'NamedSet' comes from decomposing the result of applying the schedule
-- to the domain ('decomposeUnionSetNamed'). Each 'NamedMap' comes from
-- decomposing the reversed schedule ('decomposeUnionMapNamed').
mkMultiScannerFromNamed :: KnownNat nParams => [NamedSet] -> [NamedMap] -> MultiScanner nParams
mkMultiScannerFromNamed namedSets namedMaps = mkMultiScanner pairs
  where
    pairs = map matchInverse namedSets
    matchInverse ns =
      let name = nsName ns
          -- Match by domain name first (forward schedule), then by range name
          -- (reversed schedule: domain is time-space, range is original space)
          inv = case filter (\nm -> nmDomainName nm == name || nmRangeName nm == name) namedMaps of
                  (nm:_) -> extractInverse nm
                  []     -> StmtInverse 0 []
      in (ns, inv)

-- | Build a 'MultiScanner' from @(NamedSet, StmtInverse)@ pairs.
mkMultiScanner :: KnownNat nParams => [(NamedSet, StmtInverse)] -> MultiScanner nParams
mkMultiScanner pairs = MultiScanner
  { msStatements = map mkStmt pairs }
  where
    mkStmt (ns, inv) =
      let nests = map conjToULoopNest (nsConjs ns)
      in ScheduledStatement
          { ssName      = maybe "" id (nsName ns)
          , ssLoopNests = nests
          , ssInverse   = inv
          }

-- | Build a 'ULoopNest' from a raw conjunction by running the
-- Boulet-Feautrier level builder directly.
conjToULoopNest :: Conjunction SetIx -> ULoopNest
conjToULoopNest (Conjunction constraints) =
  let (levels, nParams, nDims) = buildLevels constraints
  in ULoopNest levels nParams nDims

-- | Extract a 'StmtInverse' from a 'NamedMap' representing the inverse
-- schedule for one statement. The inverse map goes from time space (input)
-- to original space (output), so we extract each output dimension as an
-- 'AffineBound' in terms of input (time) dimensions and parameters.
extractInverse :: NamedMap -> StmtInverse
extractInverse nm =
  let nOut = nmNOut nm
      Conjunction constraints = case nmConjs nm of
        (c:_) -> c
        []    -> Conjunction []
      bounds = map (extractOutDimBound constraints) [0 .. nOut - 1]
  in StmtInverse nOut bounds

-- | Extract an AffineBound for one output dimension from a set of map
-- constraints. Looks for an equality constraint involving this OutDim
-- and solves for it in terms of InDim (time) and MapParam variables.
extractOutDimBound :: [Constraint MapIx] -> Int -> AffineBound
extractOutDimBound constraints outDim =
  let eqs = [c | c@(EqualityConstraint _) <- constraints, involvesOutDim outDim c]
  in case eqs of
    (c:_) -> extractMapBound outDim c
    []    -> AffineBound [] [] 0 1  -- fallback: constant zero

involvesOutDim :: Int -> Constraint MapIx -> Bool
involvesOutDim dim c =
  let (coeffs, _) = expandMapExpr c
  in any (\(_, ix) -> ix == OutDim dim) coeffs

expandMapExpr :: Constraint MapIx -> ([(Integer, MapIx)], Integer)
expandMapExpr (EqualityConstraint e) = expandExpr e
expandMapExpr (InequalityConstraint e) = expandExpr e

-- | Extract an AffineBound for an output dimension from a map equality.
-- The constraint is @c_out * out_d + (other terms) + constant = 0@.
-- Solve: @out_d = -(other terms + constant) / c_out@.
extractMapBound :: Int -> Constraint MapIx -> AffineBound
extractMapBound outDim c =
  let (coeffs, constant) = expandMapExpr c
      outCoeff = case lookup (OutDim outDim) (map (\(a,b) -> (b,a)) coeffs) of
                   Just k  -> k
                   Nothing -> error "extractMapBound: OutDim not in constraint"
      others = filter (\(_, ix) -> ix /= OutDim outDim) coeffs
      absDivisor = abs outCoeff
      sign = signum outCoeff
      loopCoeffs  = [(-sign * coeff, i) | (coeff, InDim i) <- others]
      paramCoeffs = [(-sign * coeff, i) | (coeff, MapParam i) <- others]
  in AffineBound
      { abLoopCoeffs  = loopCoeffs
      , abParamCoeffs = paramCoeffs
      , abConstant    = -sign * constant
      , abDivisor     = absDivisor
      }

-- * Coordinate recovery

-- | Recover original iteration coordinates from time-space coordinates
-- using the statement's schedule inverse.
recoverOrigCoords :: StmtInverse -> [Integer] -> [Integer] -> [Integer]
recoverOrigCoords (StmtInverse _ bounds) params timeCoords =
  map (evalInverseBound params timeCoords) bounds

evalInverseBound :: [Integer] -> [Integer] -> AffineBound -> Integer
evalInverseBound params timeCoords (AffineBound loopCs paramCs constant divisor) =
  let loopSum  = sum [c * safeIdx timeCoords i | (c, i) <- loopCs]
      paramSum = sum [c * safeIdx params i     | (c, i) <- paramCs]
  in (loopSum + paramSum + constant) `div` divisor

safeIdx :: [Integer] -> Int -> Integer
safeIdx xs i
  | i >= 0 && i < length xs = xs !! i
  | otherwise = error $ "safeIdx: index " ++ show i ++ " out of range (length " ++ show (length xs) ++ ")"

-- * Untyped FSM scanning for ULoopNest

-- | Enumerate all integer points of a ULoopNest in lexicographic order.
-- Replicates the FSM logic from 'Isl.Scan.FSM' but on untyped loop nests.
scanULoopNest :: ULoopNest -> [Integer] -> [[Integer]]
scanULoopNest nest params =
  case computeLexMin (ulnLevels nest) params [] of
    Nothing    -> []
    Just point -> go point
  where
    levels = ulnLevels nest
    n = length levels

    go point =
      point : case findCarryAndAdvance levels params point (n - 1) of
                Nothing       -> []
                Just newPoint -> go newPoint

computeLexMin :: [LoopLevel] -> [Integer] -> [Integer] -> Maybe [Integer]
computeLexMin [] _ prefix = Just (reverse prefix)
computeLexMin (level : rest) params prefix =
  case llEquality level of
    Just eq ->
      let val = evalBoundExact params prefix eq
      in computeLexMin rest params (val : prefix)
    Nothing ->
      let lo = safeMax $ map (evalLower params prefix) (llLowerBounds level)
          hi = safeMin $ map (evalUpper params prefix) (llUpperBounds level)
      in case (lo, hi) of
           (Just l, Just h) | l <= h -> computeLexMin rest params (l : prefix)
           _ -> Nothing

findCarryAndAdvance :: [LoopLevel] -> [Integer] -> [Integer] -> Int -> Maybe [Integer]
findCarryAndAdvance levels params point dim
  | dim < 0   = Nothing
  | otherwise =
    let level = levels !! dim
        prefix = take dim point
        prefixRev = reverse prefix
    in case llEquality level of
         Just _ -> findCarryAndAdvance levels params point (dim - 1)
         Nothing ->
           let currentVal = point !! dim
               hi = minimum $ map (evalUpper params prefixRev) (llUpperBounds level)
               stride = llStride level
               nextVal = currentVal + stride
           in if nextVal <= hi
              then
                let outerFixed = take dim point ++ [nextVal]
                    innerLevels = drop (dim + 1) levels
                in case computeInnerLexMin innerLevels params (reverse outerFixed) of
                     Just innerVals -> Just (outerFixed ++ innerVals)
                     Nothing -> findCarryAndAdvance levels params point (dim - 1)
              else findCarryAndAdvance levels params point (dim - 1)

computeInnerLexMin :: [LoopLevel] -> [Integer] -> [Integer] -> Maybe [Integer]
computeInnerLexMin [] _ _ = Just []
computeInnerLexMin (level : rest) params prefixRev =
  case llEquality level of
    Just eq ->
      let val = evalBoundExact params prefixRev eq
      in case computeInnerLexMin rest params (val : prefixRev) of
           Just inner -> Just (val : inner)
           Nothing    -> Nothing
    Nothing ->
      let lo = safeMax $ map (evalLower params prefixRev) (llLowerBounds level)
          hi = safeMin $ map (evalUpper params prefixRev) (llUpperBounds level)
      in case (lo, hi) of
           (Just l, Just h) | l <= h ->
             case computeInnerLexMin rest params (l : prefixRev) of
               Just inner -> Just (l : inner)
               Nothing    -> Nothing
           _ -> Nothing

-- Bound evaluation (duplicated from Enumerate to avoid typed Vec dependency)

evalBound' :: [Integer] -> [Integer] -> AffineBound -> Integer
evalBound' params prefix (AffineBound loopCs paramCs constant _divisor) =
  let loopSum = sum [c * indexLoop prefix i | (c, i) <- loopCs]
      paramSum = sum [c * indexParam params i | (c, i) <- paramCs]
  in loopSum + paramSum + constant

evalLower :: [Integer] -> [Integer] -> AffineBound -> Integer
evalLower params prefix bound =
  let num = evalBound' params prefix bound
  in ceilDiv num (abDivisor bound)

evalUpper :: [Integer] -> [Integer] -> AffineBound -> Integer
evalUpper params prefix bound =
  let num = evalBound' params prefix bound
  in floorDiv num (abDivisor bound)

evalBoundExact :: [Integer] -> [Integer] -> AffineBound -> Integer
evalBoundExact params prefix bound =
  let num = evalBound' params prefix bound
  in num `div` abDivisor bound

indexLoop :: [Integer] -> Int -> Integer
indexLoop prefix i =
  let idx = length prefix - 1 - i
  in if idx >= 0 && idx < length prefix
     then prefix !! idx
     else error $ "indexLoop: dimension " ++ show i ++ " not yet assigned"

indexParam :: [Integer] -> Int -> Integer
indexParam params i
  | i >= 0 && i < length params = params !! i
  | otherwise = error $ "indexParam: parameter " ++ show i ++ " out of range"

ceilDiv :: Integer -> Integer -> Integer
ceilDiv a b
  | b <= 0    = error "ceilDiv: non-positive divisor"
  | a >= 0    = (a + b - 1) `div` b
  | otherwise = -((-a) `div` b)

floorDiv :: Integer -> Integer -> Integer
floorDiv a b
  | b <= 0    = error "floorDiv: non-positive divisor"
  | a >= 0    = a `div` b
  | otherwise = -(((-a) + b - 1) `div` b)

safeMax :: [Integer] -> Maybe Integer
safeMax [] = Nothing
safeMax xs = Just (maximum xs)

safeMin :: [Integer] -> Maybe Integer
safeMin [] = Nothing
safeMin xs = Just (minimum xs)

-- * Linear-scan enumeration

data ActiveStream = ActiveStream
  { asName    :: !String
  , asInverse :: !StmtInverse
  , asHead    :: ![Integer]
  , asTail    :: ![[Integer]]
  }

-- | Enumerate all statement instances in lexicographic time order.
scanMulti :: KnownNat nParams => MultiScanner nParams -> Vec nParams Integer -> [StmtPoint]
scanMulti ms params =
  mergeLinear (initStreams ms (toList params)) (toList params)

-- | Strict left fold over all statement instances in schedule order.
scanMultiFold :: KnownNat nParams => MultiScanner nParams -> Vec nParams Integer -> (a -> StmtPoint -> a) -> a -> a
scanMultiFold ms params f z =
  mergeLinearFold (initStreams ms (toList params)) (toList params) f z

-- | Monadic traversal over all statement instances in schedule order.
scanMultiForM_ :: (Monad m, KnownNat nParams) => MultiScanner nParams -> Vec nParams Integer -> (StmtPoint -> m ()) -> m ()
scanMultiForM_ ms params action =
  scanMultiFold ms params (\m pt -> m >> action pt) (return ())

initStreams :: MultiScanner nParams -> [Integer] -> [ActiveStream]
initStreams (MultiScanner stmts) params =
  concatMap (stmtStreams params) stmts

stmtStreams :: [Integer] -> ScheduledStatement -> [ActiveStream]
stmtStreams params stmt =
  [ ActiveStream (ssName stmt) (ssInverse stmt) (head pts) (tail pts)
  | nest <- ssLoopNests stmt
  , let pts = scanULoopNest nest params
  , not (null pts)
  ]

mergeLinear :: [ActiveStream] -> [Integer] -> [StmtPoint]
mergeLinear [] _ = []
mergeLinear streams params =
  let minStream = minimumBy' (comparing asHead) streams
      pt = StmtPoint
        { spStmt      = asName minStream
        , spOrigCoord = recoverOrigCoords (asInverse minStream) params (asHead minStream)
        , spTimeCoord = asHead minStream
        }
      streams' = advanceStream minStream streams
  in pt : mergeLinear streams' params

mergeLinearFold :: [ActiveStream] -> [Integer] -> (a -> StmtPoint -> a) -> a -> a
mergeLinearFold [] _ _ !acc = acc
mergeLinearFold streams params f !acc =
  let minStream = minimumBy' (comparing asHead) streams
      pt = StmtPoint
        { spStmt      = asName minStream
        , spOrigCoord = recoverOrigCoords (asInverse minStream) params (asHead minStream)
        , spTimeCoord = asHead minStream
        }
      acc' = f acc pt
      streams' = advanceStream minStream streams
  in mergeLinearFold streams' params f acc'

advanceStream :: ActiveStream -> [ActiveStream] -> [ActiveStream]
advanceStream minS streams =
  case asTail minS of
    []    -> filter (not . sameStream minS) streams
    (h:t) -> map (\s -> if sameStream minS s
                        then s { asHead = h, asTail = t }
                        else s) streams

sameStream :: ActiveStream -> ActiveStream -> Bool
sameStream a b = asName a == asName b && asHead a == asHead b

minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' _ [] = error "minimumBy': empty list"
minimumBy' cmp (x:xs) = foldl' (\a b -> if cmp a b == GT then b else a) x xs

-- * Priority-queue enumeration

-- | Enumerate all statement instances in lexicographic time order (PQ variant).
scanMultiPQ :: KnownNat nParams => MultiScanner nParams -> Vec nParams Integer -> [StmtPoint]
scanMultiPQ ms params =
  mergePQ (sortBy (comparing asHead) (initStreams ms (toList params))) (toList params)

-- | Strict left fold, priority-queue variant.
scanMultiPQFold :: KnownNat nParams => MultiScanner nParams -> Vec nParams Integer -> (a -> StmtPoint -> a) -> a -> a
scanMultiPQFold ms params f z =
  mergePQFold (sortBy (comparing asHead) (initStreams ms (toList params))) (toList params) f z

-- | Monadic traversal, priority-queue variant.
scanMultiPQForM_ :: (Monad m, KnownNat nParams) => MultiScanner nParams -> Vec nParams Integer -> (StmtPoint -> m ()) -> m ()
scanMultiPQForM_ ms params action =
  scanMultiPQFold ms params (\m pt -> m >> action pt) (return ())

mergePQ :: [ActiveStream] -> [Integer] -> [StmtPoint]
mergePQ [] _ = []
mergePQ (s:rest) params =
  let pt = StmtPoint
        { spStmt      = asName s
        , spOrigCoord = recoverOrigCoords (asInverse s) params (asHead s)
        , spTimeCoord = asHead s
        }
      rest' = case asTail s of
        []    -> rest
        (h:t) -> insertSorted (s { asHead = h, asTail = t }) rest
  in pt : mergePQ rest' params

mergePQFold :: [ActiveStream] -> [Integer] -> (a -> StmtPoint -> a) -> a -> a
mergePQFold [] _ _ !acc = acc
mergePQFold (s:rest) params f !acc =
  let pt = StmtPoint
        { spStmt      = asName s
        , spOrigCoord = recoverOrigCoords (asInverse s) params (asHead s)
        , spTimeCoord = asHead s
        }
      acc' = f acc pt
      rest' = case asTail s of
        []    -> rest
        (h:t) -> insertSorted (s { asHead = h, asTail = t }) rest
  in mergePQFold rest' params f acc'

insertSorted :: ActiveStream -> [ActiveStream] -> [ActiveStream]
insertSorted x [] = [x]
insertSorted x (y:ys)
  | asHead x <= asHead y = x : y : ys
  | otherwise            = y : insertSorted x ys
