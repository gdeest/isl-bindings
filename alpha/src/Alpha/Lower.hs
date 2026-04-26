{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Lower an Alpha 'System' to ISL's value-level 'NamedSet'/'NamedMap'
-- representation, suitable for dependence analysis and schedule
-- validation.
--
-- Reads from inside a 'Reduce' body live in a higher-dimensional
-- iteration space (the body has extra reduction variables).  These
-- are extracted with a synthetic domain name and accompanied by
-- projection 'NamedMap's so the compile pipeline can compose them
-- into the equation space before computing dependences.
module Alpha.Lower
  ( lowerSystemV2
  , logicalName
  ) where

import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (symbolVal)

import Isl.Typed.Constraints
  ( Constraint(..), SetIx(..), MapIx(..), Conjunction(..) )
import qualified Isl.Typed.Constraints as C
import Isl.Typed.Constraints (NamedSet(..), NamedMap(..))

import qualified Alpha.Core        as V2
import           Alpha.Core.Named  (the)


-- | Per-equation lowered result: one or more statements.  A plain RHS
-- yields exactly one quad; a top-level 'V2.Case' fans out to one quad
-- per branch, all writing to the same logical array @eqName@ (range
-- name).
type LoweredQuad = (NamedSet, NamedMap, [NamedMap], [NamedMap])


-- | Recover the equation name from a lowered statement name.
--
-- The @"__br"I@ suffix is coined in 'lowerCaseBranchesV2' — this is
-- the sole inverse, consumed by schedule/reduce lookup in
-- 'Alpha.Codegen' and 'Alpha.Polyhedral.Dependence'.
logicalName :: String -> String
logicalName n
  | "__br" `isPrefixOf` suffix
  , let digits = drop 4 suffix
  , not (null digits)
  , all isDigit digits
  = prefix
  | otherwise
  = n
  where
    (prefix, suffix) = splitAtLast "__br" n
    splitAtLast needle hay = go (length hay - length needle)
      where
        go i
          | i < 0                          = (hay, "")
          | needle `isPrefixOf` drop i hay = (take i hay, drop i hay)
          | otherwise                      = go (i - 1)


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Helpers
-- ═══════════════════════════════════════════════════════════════════════

identityCs :: Int -> [Constraint MapIx]
identityCs n =
  [ EqualityConstraint (C.Add (C.Ix (OutDim k)) (C.Mul (-1) (C.Ix (InDim k))))
  | k <- [0 .. n - 1]
  ]

mapDomToMap :: [Constraint SetIx] -> [Constraint MapIx]
mapDomToMap = map (mapConstraint setToMapIx)

setToMapIx :: SetIx -> MapIx
setToMapIx (SetDim d)   = InDim d
setToMapIx (SetParam p) = MapParam p

mapConstraint :: (a -> b) -> Constraint a -> Constraint b
mapConstraint f (EqualityConstraint e)   = EqualityConstraint (fmap f e)
mapConstraint f (InequalityConstraint e) = InequalityConstraint (fmap f e)


-- ═══════════════════════════════════════════════════════════════════════
-- §3. V2 entry point — structural copy of 'Alpha.Core' Named fields
-- ═══════════════════════════════════════════════════════════════════════
--
-- The elaborator (in 'Alpha.Surface.Elaborate') has already done all
-- the type-level reflection: every Core node carries a 'Named' payload
-- with the parameter context folded in and the right shape
-- (Conjunction, nIn/nOut, dim count).  Lowering becomes a structural
-- walk that re-tags the payloads with the right tuple names and emits
-- the four lists @lowerSystemV2@ produces.

-- | Lower an elaborated 'V2.System sys a' to
-- @(domains, writes, reads, projections)@:
--
--   * @domains@     — one 'NamedSet' per equation (or per branch of a
--                     top-level 'V2.Case')
--   * @writes@      — one identity 'NamedMap' per equation\/branch
--   * @reads@       — one 'NamedMap' per array access (body-space
--                     reads use synthetic domain names)
--   * @projections@ — maps from synthetic body spaces back to
--                     equation spaces (for composing before dep
--                     computation)
--
-- The returned 'NamedSet's and 'NamedMap's get tuple names installed
-- by this walker (Elaborate leaves them anonymous), constraints come
-- straight from the elaborated 'Named' payloads, and the Case fan-out
-- and Reduce body-naming convention (@eqName__brI@ /
-- @eqName__body_K@) is the canonical one consumed by 'logicalName',
-- 'Alpha.Polyhedral.Dependence', and 'Alpha.Codegen.lowerScheduleMaps'.
lowerSystemV2
  :: forall sys a.
     V2.System sys a
  -> ([NamedSet], [NamedMap], [NamedMap], [NamedMap])
lowerSystemV2 sys =
  let params  = the (V2.sysParams sys)
      (quads, _) = lowerEqsV2 params 0 (V2.sysEqs sys)
      domains     = map (\(d, _, _, _) -> d) quads
      writes      = map (\(_, w, _, _) -> w) quads
      reads_      = concatMap (\(_, _, rs, _) -> rs) quads
      projections = concatMap (\(_, _, _, ps') -> ps') quads
  in (domains, writes, reads_, projections)

-- | Read-extraction context for the V2 walker.  No carried
-- domain-constraints field: every emitted read map already inherits
-- the pctx-folded constraints from the elaborator's
-- 'Named m IslMap' payload.
data ReadCtxV2 = ReadCtxV2
  { rcV2Params  :: ![String]
  , rcV2EqName  :: !String     -- owning equation name (for synthetic Reduce body names)
  , rcV2DomName :: !String     -- domain name installed on emitted read maps
  }

lowerEqsV2
  :: [String] -> Int -> [V2.SomeEquation sys a]
  -> ([LoweredQuad], Int)
lowerEqsV2 _      counter []       = ([], counter)
lowerEqsV2 params counter (V2.SomeEquation vdecl body : rest) =
  let eqName = V2.vdName vdecl
      nDims  = V2.vdDims vdecl
      domNS  = the (V2.vdDom vdecl)
      (quads, counter1)  = lowerOneEqV2 params counter eqName nDims domNS body
      (rest', counter2)  = lowerEqsV2   params counter1 rest
  in (quads ++ rest', counter2)

-- | Lower a single V2 equation.  Top-level 'V2.Case' fans out per
-- branch (statement name @eqName__brI@, range = @eqName@); any other
-- body emits a single quad keyed on @eqName@.
lowerOneEqV2
  :: [String] -> Int -> String -> Int -> NamedSet
  -> V2.Expr sys dom a
  -> ([LoweredQuad], Int)
lowerOneEqV2 params counter eqName nDims _domNS (V2.Case _ branches) =
  lowerCaseBranchesV2 params counter eqName nDims 0 branches
lowerOneEqV2 params counter eqName nDims domNS body =
  let domain   = retagSet (Just eqName) params nDims domNS
      writeMap = identityWriteMap params eqName eqName nDims (nsConjs domain)
      ctx0 = ReadCtxV2
        { rcV2Params  = params
        , rcV2EqName  = eqName
        , rcV2DomName = eqName
        }
      (readMaps, projs, counter') = extractReadsV2 ctx0 counter body
  in ([(domain, writeMap, readMaps, projs)], counter')

lowerCaseBranchesV2
  :: [String] -> Int -> String -> Int -> Int
  -> V2.CaseBranches sys dom bs a
  -> ([LoweredQuad], Int)
lowerCaseBranchesV2 _      counter _      _     _ V2.BNil = ([], counter)
lowerCaseBranchesV2 params counter eqName nDims i
                    (V2.BCons namedB _ body rest) =
  let brName  = eqName ++ "__br" ++ show i
      brSet   = the namedB
      domain  = retagSet (Just brName) params nDims brSet
      writeMap = identityWriteMap params brName eqName nDims (nsConjs domain)
      ctx = ReadCtxV2
        { rcV2Params  = params
        , rcV2EqName  = eqName
        , rcV2DomName = brName
        }
      (readMaps, projs, counter1) = extractReadsV2 ctx counter body
      (restQuads, counter2) =
        lowerCaseBranchesV2 params counter1 eqName nDims (i + 1) rest
  in ((domain, writeMap, readMaps, projs) : restQuads, counter2)

-- | Re-tag an elaborated 'NamedSet' with a tuple name and a fresh
-- (params, nDims) header.  Elaborate leaves @nsName = Nothing@ on
-- per-node sets and uses the declared name on 'vdDom'; 'lowerSystemV2'
-- is the canonical site for installing statement-/branch-level tuple
-- names that downstream ISL operations key on.
retagSet :: Maybe String -> [String] -> Int -> NamedSet -> NamedSet
retagSet name params nDims ns = NamedSet
  { nsName   = name
  , nsParams = params
  , nsNDims  = nDims
  , nsConjs  = nsConjs ns
  }

-- | Identity write map @stmtName[i] -> arrName[i]@, with the iteration
-- domain's conjunction folded in so ISL sees the legal write set.
identityWriteMap
  :: [String] -> String -> String -> Int
  -> [Conjunction SetIx] -> NamedMap
identityWriteMap params stmtName arrName nDims domConjs = NamedMap
  { nmDomainName = Just stmtName
  , nmRangeName  = Just arrName
  , nmParams     = params
  , nmNIn        = nDims
  , nmNOut       = nDims
  , nmConjs      = [ Conjunction (identityCs nDims ++ mapConjToMap c)
                   | c <- domConjs ]
  }

mapConjToMap :: Conjunction SetIx -> [Constraint MapIx]
mapConjToMap (Conjunction cs) = mapDomToMap cs

extractReadsV2
  :: ReadCtxV2 -> Int
  -> V2.Expr sys dom a
  -> ([NamedMap], [NamedMap], Int)
extractReadsV2 _   c (V2.Var {})    = ([], [], c)
extractReadsV2 _   c (V2.Const _)   = ([], [], c)
extractReadsV2 ctx c (V2.Pw _ e1 e2) =
  let (r1, p1, c1) = extractReadsV2 ctx c  e1
      (r2, p2, c2) = extractReadsV2 ctx c1 e2
  in (r1 ++ r2, p1 ++ p2, c2)
extractReadsV2 ctx c (V2.PMap _ e)   = extractReadsV2 ctx c e

extractReadsV2 ctx c (V2.Dep namedM _namedSrc _ inner) =
  let mNM   = the namedM
      readMap = mNM
        { nmDomainName = Just (rcV2DomName ctx)
        , nmRangeName  = Just (innerVarNameV2 inner)
        , nmParams     = rcV2Params ctx
        }
      (innerReads, innerProjs, c') = extractReadsV2 ctx c inner
  in (readMap : innerReads, innerProjs, c')

extractReadsV2 ctx c (V2.Reduce _ namedP _namedBody _ body) =
  let bodyName = rcV2EqName ctx ++ "__body_" ++ show c
      c'       = c + 1
      pNM      = the namedP
      projMap  = pNM
        { nmDomainName = Just bodyName
        , nmRangeName  = Just (rcV2EqName ctx)
        , nmParams     = rcV2Params ctx
        }
      bodyCtx = ctx { rcV2DomName = bodyName }
      (bodyReads, bodyProjs, c'') = extractReadsV2 bodyCtx c' body
  in (bodyReads, projMap : bodyProjs, c'')

extractReadsV2 ctx c (V2.Case _ branches) =
  extractBranchReadsV2 ctx c branches

extractBranchReadsV2
  :: ReadCtxV2 -> Int
  -> V2.CaseBranches sys dom bs a
  -> ([NamedMap], [NamedMap], Int)
extractBranchReadsV2 _   c V2.BNil = ([], [], c)
extractBranchReadsV2 ctx c (V2.BCons _ _ body rest) =
  -- Nested (non-top-level) 'Case' is not fanned out: walk each
  -- branch in the current ambient and accumulate reads / projs.
  -- Top-level 'Case' fan-out happens in 'lowerOneEqV2' via
  -- 'normalizeCases' upstream.
  let (bodyReads, bodyProjs, c1) = extractReadsV2 ctx c body
      (restReads, restProjs, c2) = extractBranchReadsV2 ctx c1 rest
  in (bodyReads ++ restReads, bodyProjs ++ restProjs, c2)

innerVarNameV2 :: V2.Expr sys dom a -> String
innerVarNameV2 (V2.Var (_ :: Proxy v) _ _) = symbolVal (Proxy @v)
innerVarNameV2 (V2.PMap _ e)               = innerVarNameV2 e
innerVarNameV2 (V2.Dep _ _ _ inner)        = innerVarNameV2 inner
innerVarNameV2 _                           = "<unknown>"
