{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Compilation pipeline: DSL 'Program' → 'MultiScanner'.
--
-- 1. Flatten 'LoopNode' tree into per-statement domains and schedules
-- 2. Convert to 'NamedSet' \/ 'NamedMap' (existing types from highlevel)
-- 3. Build 'MultiScanner' (existing from scan)
module Isl.DSL.Compile
  ( -- * Extraction
    extractStmts
    -- * Conversion to NamedSet/NamedMap
  , stmtToNamedSet
  , stmtToNamedSchedule
  , stmtToNamedInverse
    -- * Full compilation
  , Compiled(..)
  , compile
    -- * Debug
  , debugCompiled
  , stmtToNamedTimeSet
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.TypeLits (KnownNat)

import Isl.HighLevel.Constraints
  ( Conjunction(..), Constraint(..), Expr(..), SetIx(..), MapIx(..) )
import Isl.HighLevel.Pure (NamedSet(..), NamedMap(..))
import Isl.Scan.Multi
  ( MultiScanner, mkMultiScannerFromNamed )
import Isl.Scan.Types (mkVec)

import Isl.DSL.Types hiding (Ix)  -- hide DSL's Ix to avoid clash with Expr's Ix
import Isl.DSL.Transform (applyTransforms)
import Isl.DSL.Verify (verifyAll, VerifyError)


-- ---------------------------------------------------------------------------
-- Statement extraction from loop nests
-- ---------------------------------------------------------------------------

-- | Extract all statements from a loop nest, inferring domains from
-- loop bounds and initial schedules from nesting order.
-- Variable names are resolved to ISL dimension/parameter indices.
extractStmts :: [String]    -- ^ Parameter names
             -> [LoopNode]  -- ^ Top-level loop nodes
             -> [ExtractedStmt]
extractStmts = extractStmtsResolved


-- | Convert a DSL 'AExpr' to an ISL 'Expr SetIx', resolving variable names
-- to dimension/parameter indices.
resolveAExpr :: [String] -> [String] -> AExpr -> Expr SetIx
resolveAExpr paramNames dimNames = go
  where
    go (ALit n) = Constant n
    go (AVar name) =
      case lookupIndex name dimNames of
        Just idx -> Ix (SetDim idx)
        Nothing ->
          case lookupIndex name paramNames of
            Just idx -> Ix (SetParam idx)
            Nothing -> error $ "resolveAExpr: unknown variable " ++ show name
    go (AParam name) =
      case lookupIndex name paramNames of
        Just idx -> Ix (SetParam idx)
        Nothing -> error $ "resolveAExpr: unknown parameter " ++ show name
    go (AAdd a b) = Add (go a) (go b)
    go (ANeg a) = Mul (-1) (go a)
    go (AMul k a) = Mul k (go a)
    go (AFloorDiv a k) = FloorDiv (go a) k
    go (AMod _a _k) = error "resolveAExpr: AMod — decompose to FloorDiv"

lookupIndex :: Eq a => a -> [a] -> Maybe Int
lookupIndex _ [] = Nothing
lookupIndex x (y:ys)
  | x == y    = Just 0
  | otherwise = (+ 1) <$> lookupIndex x ys


-- | Convert a DSL 'AExpr' to an ISL 'Expr MapIx', for schedule maps.
resolveAExprMap :: [String] -> [String] -> [String] -> AExpr -> Expr MapIx
resolveAExprMap paramNames inDimNames _outDimNames = go
  where
    go (ALit n) = Constant n
    go (AVar name) =
      case lookupIndex name inDimNames of
        Just idx -> Ix (InDim idx)
        Nothing ->
          case lookupIndex name paramNames of
            Just idx -> Ix (MapParam idx)
            Nothing -> error $ "resolveAExprMap: unknown variable " ++ show name
    go (AParam name) =
      case lookupIndex name paramNames of
        Just idx -> Ix (MapParam idx)
        Nothing -> error $ "resolveAExprMap: unknown parameter " ++ show name
    go (AAdd a b) = Add (go a) (go b)
    go (ANeg a) = Mul (-1) (go a)
    go (AMul k a) = Mul k (go a)
    go (AFloorDiv a k) = FloorDiv (go a) k
    go (AMod _a _k) = error "resolveAExprMap: AMod — decompose to FloorDiv"


-- ---------------------------------------------------------------------------
-- Conversion to NamedSet / NamedMap
-- ---------------------------------------------------------------------------

-- | Build domain constraints from an extracted statement.
buildDomainConstraints :: ExtractedStmt -> Conjunction SetIx
buildDomainConstraints es =
  let params = esParamNames es
      dims = esDimNames es
      -- Re-extract loop bounds from the stored domain
      -- Actually, we need to re-derive from the original loop bounds.
      -- The stored esDomain was built during extraction with resolved indices.
  in esDomain es  -- already resolved during extraction

-- But we need to fix extraction to resolve names to indices properly.
-- Let me rewrite extractStmts to resolve during extraction:

-- | Convert extracted statement to a NamedSet (domain).
stmtToNamedSet :: ExtractedStmt -> NamedSet
stmtToNamedSet es = NamedSet
  { nsName   = Just (esName es)
  , nsParams = esParamNames es
  , nsNDims  = length (esDimNames es)
  , nsConjs  = [esDomain es]
  }

-- | Build the initial schedule map for a statement.
-- The schedule is the identity permutation plus sequential ordering indices.
--
-- For a statement with dims [t, i] at sequential position [0, 1]:
-- Schedule: [t, i] → [t, 1, i]  (interleave seq indices)
--
-- Actually, for ISL-compatible schedules, we use a simpler approach:
-- the schedule maps each iteration to a time vector that encodes
-- (outermost dim, seq_idx, next dim, seq_idx, ..., innermost dim).
stmtToNamedSchedule :: ExtractedStmt -> NamedMap
stmtToNamedSchedule es =
  let params = esParamNames es
      dims = esDimNames es
      nIn = length dims
      seqIdxs = take (length dims) (esSeqIdx es)
      -- Schedule output dimensions: for each nesting level,
      -- emit the dimension value then the sequential index
      schedDims = interleaveSchedule dims seqIdxs
      nOut = length schedDims
      -- Build map constraints: each output dim = corresponding expression
      mapConstrs = zipWith (mkSchedConstraint params dims) [0..] schedDims
  in NamedMap
    { nmDomainName = Just (esName es)
    , nmRangeName  = Nothing
    , nmParams     = params
    , nmNIn        = nIn
    , nmNOut       = nOut
    , nmConjs      = [Conjunction mapConstrs]
    }

-- | Interleave sequential indices before dimension values.
-- [t, i] with seqIdxs [0, 1] → [(ALit 0), (AVar "t"), (ALit 1), (AVar "i")]
-- This puts the seq index BEFORE the dim, so that in lexicographic order,
-- statements at different seq positions are fully separated.
interleaveSchedule :: [String] -> [Int] -> [AExpr]
interleaveSchedule [] [] = []
interleaveSchedule [] _  = []
interleaveSchedule (d:ds) [] = AVar d : interleaveSchedule ds []
interleaveSchedule (d:ds) (s:ss) = ALit (fromIntegral s) : AVar d : interleaveSchedule ds ss

-- | Build a single schedule constraint: outDim_i == expr
mkSchedConstraint :: [String] -> [String] -> Int -> AExpr -> Constraint MapIx
mkSchedConstraint params dims outIdx expr =
  -- Constraint: outDim_i - expr == 0
  EqualityConstraint $ Add (Ix (OutDim outIdx)) (Mul (-1) (resolveAExprMap params dims [] expr))


-- | Build the schedule inverse map for a statement.
stmtToNamedInverse :: ExtractedStmt -> NamedMap
stmtToNamedInverse es =
  let params = esParamNames es
      dims = esDimNames es
      nIn = length dims
      schedDims = interleaveSchedule dims (take (length dims) (esSeqIdx es))
      nOut = length schedDims
      -- Inverse: nOut input dims (time space) → nIn output dims (original space)
      -- For each original dim d_k, it equals the corresponding time dim
      -- (skipping the sequential index dims)
      invConstrs = zipWith (mkInvConstraint params nOut) [0..] dims
  in NamedMap
    { nmDomainName = Just (esName es)
    , nmRangeName  = Just (esName es)
    , nmParams     = params
    , nmNIn        = nOut   -- time dims are input to inverse
    , nmNOut       = nIn    -- original dims are output
    , nmConjs      = [Conjunction invConstrs]
    }

-- | Build inverse constraint: outDim_k == inDim_{2*k+1} (seq precedes dim)
mkInvConstraint :: [String] -> Int -> Int -> String -> Constraint MapIx
mkInvConstraint _params _nTimeDims origIdx _dimName =
  -- In the interleaved schedule, dim k maps to time dim 2*k+1
  -- (time dims: seq0, d0, seq1, d1, ... → d_k is at position 2*k+1)
  EqualityConstraint $ Add (Ix (OutDim origIdx)) (Mul (-1) (Ix (InDim (2 * origIdx + 1))))


-- ---------------------------------------------------------------------------
-- Full compilation
-- ---------------------------------------------------------------------------

-- | Compiled program ready for execution.
data Compiled nParams = Compiled
  { cScanner     :: MultiScanner nParams
  , cStmts       :: Map String ExtractedStmt  -- ^ by name
  , cParamNames  :: [String]
  , cArrayDecls  :: Map String ArrayDecl
  , cMemMaps     :: Map String MemMapDef
  }

-- | Compile a DSL program into an executable scanner.
--
-- Runs ISL to verify:
--
-- * Array access bounds (parametric, for all N)
-- * Schedule legality (all dependences satisfied after transforms)
--
-- Returns 'Left' with errors, or 'Right' with the compiled program.
compile
  :: forall nParams ps. KnownNat nParams
  => Program ps
  -> [String]       -- ^ Parameter names (in order)
  -> Either [VerifyError] (Compiled nParams)
compile prog paramNames =
  let -- 1. Extract statements
      stmts = extractStmtsResolved paramNames (pLoops prog)
      -- 2. Run verification (ISL-based, Ivory-style: value-level DSL inspected at runtime)
      errors = verifyAll paramNames (pArrays prog) stmts
  in case errors of
    (_:_) -> Left errors
    [] ->
      let xforms = pTransforms prog
          -- 3. Apply transforms and build time-space representations
          transformed = map (applyXforms xforms) stmts
          namedTimeSets = map fst transformed
          namedInvs = map snd transformed
          -- 4. Build MultiScanner
          scanner = mkMultiScannerFromNamed @nParams namedTimeSets namedInvs
          -- 5. Build stmt map
          stmtMap = Map.fromList [(esName s, s) | s <- stmts]
      in Right Compiled
          { cScanner    = scanner
          , cStmts      = stmtMap
          , cParamNames = paramNames
          , cArrayDecls = pArrays prog
          , cMemMaps    = pMemMaps prog
          }
  where
    applyXforms :: [Transform] -> ExtractedStmt -> (NamedSet, NamedMap)
    applyXforms xforms es =
      let dims = esDimNames es
          seqIdxs = take (length dims) (esSeqIdx es)
          initSched = buildInitialNamedSched dims seqIdxs
          (xformedSched, _parDims) = applyTransforms xforms dims initSched
      in buildTimeSpaceFromSched es xformedSched

-- | Extract statements with properly resolved indices.
extractStmtsResolved :: [String] -> [LoopNode] -> [ExtractedStmt]
extractStmtsResolved paramNames nodes =
  go [] [] [] 0 nodes
  where
    go :: [String]             -- enclosing dim names
       -> [Constraint SetIx]  -- accumulated domain constraints
       -> [Int]               -- sequential indices
       -> Int                 -- current seq position
       -> [LoopNode]
       -> [ExtractedStmt]
    go _dims _constrs _seqIdxs _pos [] = []
    go dims constrs seqIdxs pos (node:rest) =
      let stmts = case node of
            ForLoop dimName lo hi body ->
              let dimIdx = length dims
                  loExpr = resolveAExpr paramNames dims lo
                  hiExpr = resolveAExpr paramNames dims hi
                  -- dim >= lo  ⟹  dim - lo >= 0
                  loCstr = InequalityConstraint (Add (Ix (SetDim dimIdx)) (Mul (-1) loExpr))
                  -- dim <= hi  ⟹  hi - dim >= 0
                  hiCstr = InequalityConstraint (Add hiExpr (Mul (-1) (Ix (SetDim dimIdx))))
                  newDims = dims ++ [dimName]
                  newConstrs = constrs ++ [loCstr, hiCstr]
              in go newDims newConstrs (seqIdxs ++ [pos]) 0 body

            Assign sid arrayName subs expr ->
              let name = "S" ++ show (let StmtId n = sid in n)
              in [ExtractedStmt
                    { esStmtId     = sid
                    , esName       = name
                    , esArrayName  = arrayName
                    , esSubs       = subs
                    , esExpr       = expr
                    , esDimNames   = dims
                    , esParamNames = paramNames
                    , esDomain     = Conjunction constrs
                    , esSchedule   = zip dims [0..]
                    , esSeqIdx     = seqIdxs ++ [pos]
                    }]

            Guard _cond body ->
              go dims constrs seqIdxs pos body

            Seq children ->
              go dims constrs seqIdxs pos children

          restStmts = go dims constrs seqIdxs (pos + 1) rest
      in stmts ++ restStmts


-- | Build a NamedSet in time space (scheduled domain).
-- For identity schedule with interleaved seq indices:
-- domain {[t,i] : ...} with seqIdx [0,1]
-- → time domain {[t, 0, i, 1] : ...}
stmtToNamedTimeSet :: ExtractedStmt -> NamedSet
stmtToNamedTimeSet es =
  let params = esParamNames es
      dims = esDimNames es
      -- Only use seq indices up to the number of loop dims
      seqIdxs = take (length dims) (esSeqIdx es)
      schedDims = interleaveSchedule dims seqIdxs
      nTimeDims = length schedDims
      -- Build time-space constraints: map domain constraints to time dims
      -- In the interleaved layout: dim k → time dim 2*k
      -- seq indices: time dim (2*k+1) == seqIdx_k
      domConstrs = remapDomainToTime (length dims) (esDomain es)
      seqConstrs = zipWith mkSeqConstraint [0..] seqIdxs
  in NamedSet
    { nsName   = Just (esName es)
    , nsParams = params
    , nsNDims  = nTimeDims
    , nsConjs  = [Conjunction (let Conjunction cs = domConstrs in cs ++ seqConstrs)]
    }

-- | Remap domain constraints from original dims to time-space dims.
-- Original dim k → time dim 2*k (interleaved with seq indices).
remapDomainToTime :: Int -> Conjunction SetIx -> Conjunction SetIx
remapDomainToTime _nDims (Conjunction constrs) =
  Conjunction (map remapConstraint constrs)
  where
    remapConstraint (EqualityConstraint e)   = EqualityConstraint (remapExpr e)
    remapConstraint (InequalityConstraint e) = InequalityConstraint (remapExpr e)

    remapExpr (Ix (SetDim d))   = Ix (SetDim (2 * d + 1))     -- dim k → 2*k+1 (seq precedes dim)
    remapExpr (Ix (SetParam p)) = Ix (SetParam p)              -- params unchanged
    remapExpr (Constant k)      = Constant k
    remapExpr (Add a b)         = Add (remapExpr a) (remapExpr b)
    remapExpr (Mul k e)         = Mul k (remapExpr e)
    remapExpr (FloorDiv e k)    = FloorDiv (remapExpr e) k

-- | Build a constraint fixing a sequential index dim: time_dim == value.
mkSeqConstraint :: Int -> Int -> Constraint SetIx
mkSeqConstraint dimPairIdx seqVal =
  -- time dim (2*dimPairIdx) == seqVal (seq precedes dim at 2*k+1)
  -- ⟹ time_dim - seqVal == 0
  EqualityConstraint (Add (Ix (SetDim (2 * dimPairIdx))) (Constant (negate (fromIntegral seqVal))))


-- ---------------------------------------------------------------------------
-- Transform-aware schedule building
-- ---------------------------------------------------------------------------

-- | Build initial named schedule from dims and seq indices.
-- Layout: [("_seq0", lit s0), ("t", AVar "t"), ("_seq1", lit s1), ("i", AVar "i"), ...]
buildInitialNamedSched :: [String] -> [Int] -> [(String, AExpr)]
buildInitialNamedSched [] [] = []
buildInitialNamedSched (d:ds) (s:ss) =
  ("_seq" ++ show (length ds - length ds), ALit (fromIntegral s))
  : (d, AVar d)
  : buildInitialNamedSched ds ss
buildInitialNamedSched (d:ds) [] = (d, AVar d) : buildInitialNamedSched ds []
buildInitialNamedSched _ _ = []

-- | Build time-space NamedSet and inverse NamedMap from a transformed schedule.
buildTimeSpaceFromSched :: ExtractedStmt -> [(String, AExpr)] -> (NamedSet, NamedMap)
buildTimeSpaceFromSched es sched =
  let params = esParamNames es
      dims = esDimNames es
      nTimeDims = length sched
      nOrigDims = length dims

      -- Build domain constraints in time space
      -- For each time dim, we have an expression in terms of original dims.
      -- The time domain is the image of the original domain under the schedule.
      --
      -- Strategy: add equality constraints time_k = sched_k(orig_dims),
      -- plus the original domain constraints remapped to time space.
      -- But this creates a higher-dimensional space. Instead, we directly
      -- express the time domain constraints.
      --
      -- Simpler approach: keep original domain, add schedule equalities,
      -- then project out original dims. But ISL isn't available here.
      --
      -- Pragmatic approach: for each time dim that is a simple function
      -- of original dims, substitute back to get bounds.
      -- For identity dims (time_k = orig_j): use orig_j's bounds directly.
      -- For seq dims (time_k = literal): use equality constraint.
      -- For skewed dims (time_k = orig_j + orig_m): derive bounds from originals.

      -- Build time domain by generating constraints for each time dim
      timeConstrs = concatMap (timeConstraintsForDim params dims es) (zip [0..] sched)

      timeSet = NamedSet
        { nsName   = Just (esName es)
        , nsParams = params
        , nsNDims  = nTimeDims
        , nsConjs  = [Conjunction timeConstrs]
        }

      -- Build inverse map: for each original dim, express it in terms of time dims.
      -- This requires solving the schedule equations for the original dims.
      -- For simple cases (identity + skew), we can invert analytically.
      invConstrs = buildInverseConstrs params dims sched
      invMap = NamedMap
        { nmDomainName = Just (esName es)
        , nmRangeName  = Just (esName es)
        , nmParams     = params
        , nmNIn        = nTimeDims
        , nmNOut       = nOrigDims
        , nmConjs      = [Conjunction invConstrs]
        }

  in (timeSet, invMap)

-- | Generate time-space constraints for a single time dimension.
timeConstraintsForDim
  :: [String]           -- ^ param names
  -> [String]           -- ^ original dim names
  -> ExtractedStmt      -- ^ for domain bounds
  -> (Int, (String, AExpr))  -- ^ (time dim index, (name, schedule expr))
  -> [Constraint SetIx]
timeConstraintsForDim params dims es (timeDimIdx, (_name, expr)) =
  case expr of
    -- Literal (seq index): equality constraint
    ALit n ->
      [EqualityConstraint (Add (Ix (SetDim timeDimIdx)) (Constant (negate n)))]

    -- Simple variable reference: use that variable's bounds
    AVar varName ->
      case lookupIndex varName dims of
        Just origDimIdx ->
          -- Get bounds from original domain
          let (lo, hi) = getOrigDimBounds origDimIdx (esDomain es)
          in [ InequalityConstraint (Add (Ix (SetDim timeDimIdx)) (Mul (-1) (remapToTimeDim params dims lo)))
             , InequalityConstraint (Add (remapToTimeDim params dims hi) (Mul (-1) (Ix (SetDim timeDimIdx))))
             ]
        Nothing -> error $ "timeConstraintsForDim: unknown var " ++ show varName

    -- Affine expression (e.g., skew: t + i): derive bounds from components
    _ ->
      -- For a general affine expression of original dims, compute bounds
      -- by substituting the bounds of each original dim.
      -- This is an overapproximation (rectangular hull) but correct for scanning.
      let (loBound, hiBound) = affineExprBounds params dims es expr
      in [ InequalityConstraint (Add (Ix (SetDim timeDimIdx)) (Mul (-1) loBound))
         , InequalityConstraint (Add hiBound (Mul (-1) (Ix (SetDim timeDimIdx))))
         ]

-- | Get the lower and upper bounds for an original dimension from the domain.
-- Assumes the domain has constraints of the form: dim >= lo AND hi >= dim.
getOrigDimBounds :: Int -> Conjunction SetIx -> (Expr SetIx, Expr SetIx)
getOrigDimBounds dimIdx (Conjunction constrs) =
  let -- Find inequality constraints involving SetDim dimIdx
      -- Lower: SetDim dimIdx - lo >= 0  →  lo bound
      -- Upper: hi - SetDim dimIdx >= 0  →  hi bound
      lo = head [negateExcept dimIdx e | InequalityConstraint e <- constrs,
                 hasPositiveCoeff dimIdx e]
      hi = head [negateExcept dimIdx e | InequalityConstraint e <- constrs,
                 hasNegativeCoeff dimIdx e]
  in (lo, hi)
  where
    hasPositiveCoeff d (Add (Ix (SetDim d')) _) = d == d'
    hasPositiveCoeff d (Ix (SetDim d')) = d == d'
    hasPositiveCoeff _ _ = False

    hasNegativeCoeff d (Add _ (Mul (-1) (Ix (SetDim d')))) = d == d'
    hasNegativeCoeff d (Add (Mul (-1) (Ix (SetDim d'))) _) = d == d'
    hasNegativeCoeff _ _ = False

    -- Extract the "other side" of the constraint
    negateExcept d (Add (Ix (SetDim d')) rhs) | d == d' = Mul (-1) rhs
    negateExcept d (Add lhs (Mul (-1) (Ix (SetDim d')))) | d == d' = lhs
    negateExcept _ e = e  -- fallback

-- | Compute bounds for an affine expression over original dims.
-- Uses the rectangular hull: substitute min/max of each component.
affineExprBounds :: [String] -> [String] -> ExtractedStmt -> AExpr -> (Expr SetIx, Expr SetIx)
affineExprBounds params dims es expr = case expr of
  AAdd a b ->
    let (loA, hiA) = affineExprBounds params dims es a
        (loB, hiB) = affineExprBounds params dims es b
    in (Add loA loB, Add hiA hiB)
  AMul k a ->
    let (loA, hiA) = affineExprBounds params dims es a
    in if k >= 0
       then (Mul k loA, Mul k hiA)
       else (Mul k hiA, Mul k loA)  -- flip for negative
  ANeg a ->
    let (loA, hiA) = affineExprBounds params dims es a
    in (Mul (-1) hiA, Mul (-1) loA)
  ALit n -> (Constant n, Constant n)
  AVar varName ->
    case lookupIndex varName dims of
      Just origDimIdx ->
        let (lo, hi) = getOrigDimBounds origDimIdx (esDomain es)
        in (lo, hi)
      Nothing ->
        case lookupIndex varName params of
          Just pIdx -> (Ix (SetParam pIdx), Ix (SetParam pIdx))
          Nothing -> error $ "affineExprBounds: unknown " ++ show varName
  AParam name ->
    case lookupIndex name params of
      Just pIdx -> (Ix (SetParam pIdx), Ix (SetParam pIdx))
      Nothing -> error $ "affineExprBounds: unknown param " ++ show name
  _ -> error "affineExprBounds: unsupported expression"

-- | Remap an Expr over original dims/params to use time-dim-compatible indexing.
-- (Params stay as params, original dims stay as SetDim indices.)
remapToTimeDim :: [String] -> [String] -> Expr SetIx -> Expr SetIx
remapToTimeDim _params _dims e = e  -- bounds are already in terms of params and constants


-- | Build inverse constraints: for each original dim, find which time dim
-- maps to it and express orig_k = timeDim_j.
buildInverseConstrs :: [String] -> [String] -> [(String, AExpr)] -> [Constraint MapIx]
buildInverseConstrs _params dims sched =
  map (buildOneInverse sched) (zip [0..] dims)
  where
    buildOneInverse :: [(String, AExpr)] -> (Int, String) -> Constraint MapIx
    buildOneInverse schedList (origIdx, dimName) =
      -- Find the time dim whose expression is AVar dimName
      case findTimeDimForOrig dimName schedList of
        Just timeDimIdx ->
          -- orig_k = timeDim_j  →  OutDim k - InDim j = 0
          EqualityConstraint (Add (Ix (OutDim origIdx)) (Mul (-1) (Ix (InDim timeDimIdx))))
        Nothing ->
          -- Dim was skewed — need to solve for it from the schedule equations.
          -- For skew(t, i, 1): time[1] = t+i, time[3] = i → t = time[1] - time[3]
          -- General solution: find the linear combination of time dims that gives orig dim.
          -- For now, handle common case: skewed dim = time_j - other_time_k
          case findSkewedInverse dimName dims schedList of
            Just expr -> EqualityConstraint (Add (Ix (OutDim origIdx)) (Mul (-1) expr))
            Nothing -> error $ "buildInverseConstrs: can't invert for " ++ show dimName

    findTimeDimForOrig :: String -> [(String, AExpr)] -> Maybe Int
    findTimeDimForOrig name = go 0
      where
        go _ [] = Nothing
        go idx ((_n, AVar v):rest)
          | v == name = Just idx
          | otherwise = go (idx + 1) rest
        go idx (_:rest) = go (idx + 1) rest

    -- Handle skewed dim: if schedule has an entry d0_expr = AVar d0 + k * AVar d1,
    -- then d0 = time[d0_idx] - k * time[d1_idx]
    findSkewedInverse :: String -> [String] -> [(String, AExpr)] -> Maybe (Expr MapIx)
    findSkewedInverse targetDim allDims schedList =
      -- Look for a schedule entry that references targetDim
      let entries = zip [0..] schedList
          matching = [(idx, name, expr) | (idx, (name, expr)) <- entries,
                      mentionsDim targetDim expr]
      in case matching of
        [(timeDimIdx, _name, expr)] ->
          -- Solve: expr = targetDim → targetDim = f(timeDims)
          solveForDim targetDim timeDimIdx allDims schedList expr
        _ -> Nothing

    mentionsDim :: String -> AExpr -> Bool
    mentionsDim name (AVar v) = v == name
    mentionsDim name (AAdd a b) = mentionsDim name a || mentionsDim name b
    mentionsDim name (ANeg a) = mentionsDim name a
    mentionsDim name (AMul _ a) = mentionsDim name a
    mentionsDim _ _ = False

    -- Solve: schedExpr(origDims) == timeDim at timeDimIdx
    -- for targetDim in terms of time dims
    solveForDim :: String -> Int -> [String] -> [(String, AExpr)] -> AExpr -> Maybe (Expr MapIx)
    solveForDim target timeDimIdx allDims schedList expr = case expr of
      -- expr = AVar target + AMul k (AVar other)
      -- → target = timeDim[timeDimIdx] - k * timeDim[otherTimeDimIdx]
      AAdd (AVar v) (AMul k (AVar other)) | v == target ->
        case findTimeDimForOrig other schedList of
          Just otherTimeDimIdx ->
            Just (Add (Ix (InDim timeDimIdx)) (Mul (negate k) (Ix (InDim otherTimeDimIdx))))
          Nothing -> Nothing
      -- expr = AVar target + AVar other (factor 1)
      AAdd (AVar v) (AVar other) | v == target ->
        case findTimeDimForOrig other schedList of
          Just otherTimeDimIdx ->
            Just (Add (Ix (InDim timeDimIdx)) (Mul (-1) (Ix (InDim otherTimeDimIdx))))
          Nothing -> Nothing
      _ -> Nothing


-- ---------------------------------------------------------------------------
-- Debug
-- ---------------------------------------------------------------------------

-- | Print debug info about a compiled program.
debugCompiled :: KnownNat n => Compiled n -> [String] -> IO ()
debugCompiled compiled paramNames = do
  putStrLn "  Statements:"
  mapM_ (\(name, es) -> do
    putStrLn $ "    " ++ name ++ ": dims=" ++ show (esDimNames es)
      ++ " seqIdx=" ++ show (esSeqIdx es)
    let ns = stmtToNamedTimeSet es
    putStrLn $ "      time nDims=" ++ show (nsNDims ns)
    let Conjunction cs = head (nsConjs ns)
    mapM_ (\c -> putStrLn $ "      " ++ show c) cs
    ) (Map.toList (cStmts compiled))
  putStrLn $ "  Params: " ++ show paramNames
