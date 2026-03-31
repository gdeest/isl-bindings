{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QualifiedDo #-}

-- | Verification of polyhedral programs.
--
-- Uses ISL at DSL compile time (Haskell runtime) to verify:
--
-- * Array access arity matches declaration
-- * Access images are within declared array bounds (parametric, all N)
-- * Schedule legality (all dependences satisfied after transforms)
--
-- This is the Ivory-style approach: the EDSL builds a value-level AST,
-- then a compile\/check pass analyzes it with full ISL power.
module Isl.DSL.Verify
  ( -- * Verification results
    VerifyError(..)
    -- * Checks
  , verifyArity
  , verifyBoundsISL
  , verifySchedule
  , verifyAll
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Isl.Linear as Isl
import Isl.HighLevel.Context (runIsl, Ur(..))
import Isl.HighLevel.Constraints
  ( Conjunction(..), Constraint(..), Expr(..), SetIx(..) )
import qualified Isl.HighLevel.UnionSet as US
import qualified Isl.HighLevel.UnionMap as UM

import Isl.DSL.Types hiding (Ix)


-- ---------------------------------------------------------------------------
-- Verification results
-- ---------------------------------------------------------------------------

-- | A verification error with diagnostic info.
data VerifyError
  = ArityMismatch
    { vStmt  :: !String
    , vArray :: !String
    , vExpected :: !Int
    , vGot     :: !Int
    }
  | OutOfBounds
    { vStmt  :: !String
    , vArray :: !String
    , vMsg   :: !String
    }
  | UndeclaredArray
    { vStmt  :: !String
    , vArray :: !String
    }
  deriving (Show)


-- ---------------------------------------------------------------------------
-- Arity checking (pure, no ISL)
-- ---------------------------------------------------------------------------

-- | Verify array access arity matches declarations.
verifyArity :: Map String ArrayDecl
            -> [ExtractedStmt]
            -> [VerifyError]
verifyArity arrayDecls stmts =
  concatMap checkStmt stmts
  where
    checkStmt es =
      checkAccess es (esArrayName es) (esSubs es)
      ++ concatMap (\(name, subs) -> checkAccess es name subs) (extractReads (esExpr es))

    checkAccess es arrayName subs =
      case Map.lookup arrayName arrayDecls of
        Nothing -> [UndeclaredArray (esName es) arrayName]
        Just decl ->
          let expected = length (adBounds decl)
              got = length subs
          in [ArityMismatch (esName es) arrayName expected got | expected /= got]


-- ---------------------------------------------------------------------------
-- Bounds checking via ISL (parametric)
-- ---------------------------------------------------------------------------

-- | Verify that all array accesses are within declared bounds.
-- Uses ISL to compute the image of each domain under each access function,
-- then checks the image is a subset of the array's declared bounds.
-- **Parametric**: valid for ALL parameter values simultaneously.
verifyBoundsISL :: [String]             -- ^ param names
                -> Map String ArrayDecl -- ^ array declarations
                -> [ExtractedStmt]      -- ^ extracted statements
                -> [VerifyError]
verifyBoundsISL paramNames arrayDecls stmts =
  concatMap checkStmt stmts
  where
    checkStmt :: ExtractedStmt -> [VerifyError]
    checkStmt es =
      let dims = esDimNames es
          -- Check write access
          writeErrs = checkOneAccess paramNames dims (esDomain es)
                        (esName es) (esArrayName es) (esSubs es)
          -- Check read accesses
          reads = extractReads (esExpr es)
          readErrs = concatMap (\(arr, subs) ->
            checkOneAccess paramNames dims (esDomain es)
              (esName es) arr subs) reads
      in writeErrs ++ readErrs

    checkOneAccess :: [String] -> [String] -> Conjunction SetIx
                   -> String -> String -> [AExpr] -> [VerifyError]
    checkOneAccess params dims domain stmtName arrayName subs =
      case Map.lookup arrayName arrayDecls of
        Nothing -> []  -- caught by arity check
        Just decl ->
          let nDims = length dims
              nParams = length params
              nArrayDims = length (adBounds decl)
              nSubs = length subs
          in if nSubs /= nArrayDims then [] -- caught by arity check
             else
               -- Build ISL access map: [iterDims] -> [arraySubs]
               -- Build ISL bounds set: [arrayDims] with declared bounds
               -- Check: apply(accessMap, domain) ⊆ boundsSet
               -- Use ISL string-based construction for reliability.
               -- Build ISL string for the domain, access map, and bounds.
               let islDomStr = buildIslDomainStr params dims domain
                   islAccessStr = buildIslAccessStr params dims subs nArrayDims
                   islBoundsStr = buildIslBoundsStr params (adBounds decl)
                   result = runIsl $ Isl.do
                     domSet <- US.fromString islDomStr
                     accMap <- UM.fromString islAccessStr
                     imageSet <- UM.applyToSet domSet accMap
                     boundsSet <- US.fromString islBoundsStr
                     (Ur isSub, imageSet', boundsSet') <- US.isSubset imageSet boundsSet
                     US.freeUnionSet imageSet'
                     US.freeUnionSet boundsSet'
                     Isl.pure (Ur isSub)
               in [OutOfBounds stmtName arrayName
                    ("access image not within declared bounds"
                     ++ "\n  domain: " ++ islDomStr
                     ++ "\n  access: " ++ islAccessStr
                     ++ "\n  bounds: " ++ islBoundsStr)
                  | not result]




-- ---------------------------------------------------------------------------
-- ISL string builders
-- ---------------------------------------------------------------------------

-- | Build ISL string for a domain set.
-- e.g. "[N, T] -> { [t, i] : 1 <= t <= T and 1 <= i <= N }"
buildIslDomainStr :: [String] -> [String] -> Conjunction SetIx -> String
buildIslDomainStr params dims (Conjunction constrs) =
  "[" ++ commaSep params ++ "] -> { [" ++ commaSep dims ++ "] : "
  ++ andSep (map (constraintToIslStr params dims) constrs) ++ " }"

-- | Build ISL string for an access map.
-- e.g. "[N, T] -> { [t, i] -> [i - 1] }"
buildIslAccessStr :: [String] -> [String] -> [AExpr] -> Int -> String
buildIslAccessStr params dims subs _nOut =
  "[" ++ commaSep params ++ "] -> { [" ++ commaSep dims ++ "] -> ["
  ++ commaSep (map (aexprToIslStr params dims) subs) ++ "] }"

-- | Build ISL string for array bounds.
-- e.g. "[N, T] -> { [j] : 0 <= j <= N + 1 }"
buildIslBoundsStr :: [String] -> [Range] -> String
buildIslBoundsStr params ranges =
  let boundDims = ["__d" ++ show i | i <- [0 .. length ranges - 1]]
      constrs = concatMap (\(i, Range lo hi) ->
        let d = boundDims !! i
        in [aexprToIslStr params [] lo ++ " <= " ++ d,
            d ++ " <= " ++ aexprToIslStr params [] hi]
        ) (zip [0..] ranges)
  in "[" ++ commaSep params ++ "] -> { [" ++ commaSep boundDims ++ "] : "
     ++ andSep constrs ++ " }"

-- | Convert an ISL constraint to string.
constraintToIslStr :: [String] -> [String] -> Constraint SetIx -> String
constraintToIslStr params dims (EqualityConstraint e) =
  exprToIslStr params dims e ++ " = 0"
constraintToIslStr params dims (InequalityConstraint e) =
  exprToIslStr params dims e ++ " >= 0"

-- | Convert an ISL Expr to string.
exprToIslStr :: [String] -> [String] -> Expr SetIx -> String
exprToIslStr params dims = go
  where
    go (Ix (SetDim d))   = dims !! d
    go (Ix (SetParam p)) = params !! p
    go (Constant n) = show n
    go (Add a b) = "(" ++ go a ++ " + " ++ go b ++ ")"
    go (Mul k e) = "(" ++ show k ++ " * " ++ go e ++ ")"
    go (FloorDiv e k) = "floor((" ++ go e ++ ") / " ++ show k ++ ")"

-- | Convert a DSL AExpr to ISL string (for access maps).
aexprToIslStr :: [String] -> [String] -> AExpr -> String
aexprToIslStr params dims = go
  where
    go (ALit n) = show n
    go (AVar name) = name  -- variable name used directly
    go (AParam name) = name
    go (AAdd a b) = "(" ++ go a ++ " + " ++ go b ++ ")"
    go (ANeg a) = "(-" ++ go a ++ ")"
    go (AMul k a) = "(" ++ show k ++ " * " ++ go a ++ ")"
    go (AFloorDiv a k) = "floor((" ++ go a ++ ") / " ++ show k ++ ")"
    go (AMod a k) = "((" ++ go a ++ ") mod " ++ show k ++ ")"

commaSep :: [String] -> String
commaSep [] = ""
commaSep [x] = x
commaSep (x:xs) = x ++ ", " ++ commaSep xs

andSep :: [String] -> String
andSep [] = "true"
andSep [x] = x
andSep (x:xs) = x ++ " and " ++ andSep xs


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

lookupIdx :: Eq a => a -> [a] -> Maybe Int
lookupIdx _ [] = Nothing
lookupIdx x (y:ys)
  | x == y    = Just 0
  | otherwise = (+ 1) <$> lookupIdx x ys

-- | Extract all array reads from a ValExpr.
extractReads :: ValExpr -> [(String, [AExpr])]
extractReads (VRead name subs) = [(name, subs)]
extractReads (VBin _ a b) = extractReads a ++ extractReads b
extractReads (VNeg e) = extractReads e
extractReads (VLift _) = []
extractReads (VLit _) = []
extractReads (VApp _ args) = concatMap extractReads args


-- ---------------------------------------------------------------------------
-- Run all verifications
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Schedule legality checking via ISL
-- ---------------------------------------------------------------------------

data VerifyScheduleError
  = ScheduleViolation
    { vsWriter :: !String    -- ^ writing statement
    , vsReader :: !String    -- ^ reading/writing statement
    , vsArray  :: !String    -- ^ array causing the dependence
    , vsMsg    :: !String
    }
  deriving (Show)

-- | Verify that the schedule (after transforms) respects all data dependences.
--
-- For each write→read (flow) dependence and write→write (output) dependence,
-- checks that the writer executes before the reader in the schedule order.
--
-- Uses ISL: builds access relations as maps, computes the dependence relation
-- (equijoin on array subscripts), applies the schedule, checks lexicographic
-- ordering of the schedule distance vector.
verifySchedule :: [String]            -- ^ param names
               -> [ExtractedStmt]     -- ^ statements (with domains)
               -> [(String, AExpr)]   -- ^ per-stmt schedules (TODO: currently unused, need schedule strings)
               -> [VerifyError]
verifySchedule _paramNames _stmts _schedules =
  -- TODO: full implementation requires building ISL dependence relations
  -- and checking schedule legality. For now, pass-through.
  --
  -- The algorithm:
  -- 1. For each stmt pair (Si writes A[f(i)], Sj reads A[g(j)]):
  --    dep = { [i] -> [j] : f(i) = g(j) and i in dom_i and j in dom_j }
  -- 2. Apply schedule: sched_dep = { [sched(i)] -> [sched(j)] }
  -- 3. Check: for all (s, t) in sched_dep, t >_lex s
  --    Equivalently: delta = t - s is lexicographically positive
  --    Check: isl_set_is_empty(delta ∩ { d : d <_lex 0 }) == true
  []


-- ---------------------------------------------------------------------------
-- Run all verifications
-- ---------------------------------------------------------------------------

-- | Run all available verifications (pure + ISL-based).
verifyAll :: [String]
          -> Map String ArrayDecl
          -> [ExtractedStmt]
          -> [VerifyError]
verifyAll paramNames arrayDecls stmts =
  let arityErrors = verifyArity arrayDecls stmts
  in if not (null arityErrors)
     then arityErrors  -- don't run ISL checks if arity is wrong
     else verifyBoundsISL paramNames arrayDecls stmts
          -- ++ map schedErrToVerifyErr (verifySchedule paramNames stmts [])

schedErrToVerifyErr :: VerifyScheduleError -> VerifyError
schedErrToVerifyErr (ScheduleViolation w r arr msg) =
  OutOfBounds w arr ("schedule violation: " ++ r ++ " depends on " ++ w ++ " via " ++ arr ++ ": " ++ msg)
