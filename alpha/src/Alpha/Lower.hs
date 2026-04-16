{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

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
  ( lowerSystem
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal, symbolVal, type (+))

import Isl.Typed.Constraints
  ( Constraint(..), SetIx(..), MapIx(..), Conjunction(..) )
import qualified Isl.Typed.Constraints as C
import Isl.Typed.Params (KnownSymbols, symbolVals)
import Isl.Typed.Constraints (NamedSet(..), NamedMap(..))
import Isl.TypeLevel.Reflection
  ( DomTag, EffectiveDomTag, reflectDomConstraints )
import Isl.TypeLevel.Sing
  ( knownConstraints, reifySTConstraintsMapSplit )

import Alpha.Core


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Public API
-- ═══════════════════════════════════════════════════════════════════════

-- | Lower a system to the value-level ISL representation.
--
-- Returns @(domains, writes, reads, projections)@:
--
--   * @domains@     — one 'NamedSet' per equation
--   * @writes@      — one identity 'NamedMap' per equation
--   * @reads@       — one 'NamedMap' per array access (body-space
--                     reads use synthetic domain names)
--   * @projections@ — maps from synthetic body spaces back to
--                     equation spaces (for composing before dep
--                     computation)
lowerSystem
  :: forall ps inputs outputs locals.
     KnownSymbols ps
  => System ps inputs outputs locals
  -> ([NamedSet], [NamedMap], [NamedMap], [NamedMap])
lowerSystem (System (Decls ins outs locs) eqs) =
  let params = symbolVals @ps
      -- Each declared variable's rank and domain constraints.  Extracted
      -- up front from 'MkDecl' (which carries 'KnownNat' + 'KnownDom')
      -- rather than re-derived from the body — a 'Const' body carries
      -- neither dict in its constructor.
      declInfos = declListDomInfos ins
               <> declListDomInfos outs
               <> declListDomInfos locs
      (quads, _) = lowerEqList declInfos params 0 eqs
      domains     = map (\(d, _, _, _) -> d) quads
      writes      = map (\(_, w, _, _) -> w) quads
      reads_      = concatMap (\(_, _, rs, _) -> rs) quads
      projections = concatMap (\(_, _, _, ps') -> ps') quads
  in (domains, writes, reads_, projections)

-- | Extract @(rank, domain-constraints)@ for every declared variable
-- in a 'DeclList'.  Pattern-matching each 'MkDecl' brings the
-- 'KnownNat' / 'KnownDom' dicts into scope so we can reflect them.
declListDomInfos
  :: forall ps ds. DeclList ps ds -> Map String (Int, [Constraint SetIx])
declListDomInfos Nil = Map.empty
declListDomInfos ((MkDecl :: Decl ps d) :> rest) =
  Map.insert
    (symbolVal (Proxy @(DeclName d)))
    ( fromIntegral (natVal (Proxy @(DeclDims d)))
    , reflectDomConstraints @ps @(DeclDims d) @(DeclDomTag d)
    )
    (declListDomInfos rest)


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Read context
-- ═══════════════════════════════════════════════════════════════════════

-- | Context for the read-extraction walk.
data ReadCtx = ReadCtx
  { rcParams  :: ![String]            -- parameter names
  , rcEqName  :: !String              -- owning equation name
  , rcDomCs   :: ![Constraint SetIx]  -- current domain constraints
  , rcDomName :: !String              -- domain name for NamedMaps
  , rcNDims   :: !Int                 -- dims in rcDomName's space
  }


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Equation list walk
-- ═══════════════════════════════════════════════════════════════════════

lowerEqList
  :: forall ps decls defined.
     Map String (Int, [Constraint SetIx])
  -> [String] -> Int -> EqList ps decls defined
  -> ([(NamedSet, NamedMap, [NamedMap], [NamedMap])], Int)
lowerEqList _ _ counter EqNil = ([], counter)
lowerEqList declInfos params counter (Defines (Proxy :: Proxy name) body :& rest) =
  let eqName = symbolVal (Proxy @name)
      (nDims, domCs) = case Map.lookup eqName declInfos of
        Just info -> info
        Nothing   -> error $
          "Alpha.Lower: equation '" ++ eqName ++
          "' has no matching declaration (DefinesAllExactlyOnce invariant violated)"
      (quad, counter') = lowerOneEq params counter eqName nDims domCs body
      (quads, counter'') = lowerEqList declInfos params counter' rest
  in (quad : quads, counter'')

lowerOneEq
  :: forall ps decls n (d :: DomTag ps n) a.
     [String] -> Int -> String
  -> Int -> [Constraint SetIx]  -- rank + domain constraints from decl
  -> Expr ps decls n d a
  -> ((NamedSet, NamedMap, [NamedMap], [NamedMap]), Int)
lowerOneEq params counter eqName nDims domCs body =
  let domain  = NamedSet
        { nsName   = Just eqName
        , nsParams = params
        , nsNDims  = nDims
        , nsConjs  = [Conjunction domCs]
        }
      writeMap = NamedMap
        { nmDomainName = Just eqName
        , nmRangeName  = Just eqName
        , nmParams     = params
        , nmNIn        = nDims
        , nmNOut       = nDims
        , nmConjs      = [Conjunction (identityCs nDims ++ mapDomToMap domCs)]
        }
      ctx0 = ReadCtx
        { rcParams  = params
        , rcEqName  = eqName
        , rcDomCs   = domCs
        , rcDomName = eqName
        , rcNDims   = nDims
        }
      (readMaps, projs, counter') = extractReads ctx0 counter body
  in ((domain, writeMap, readMaps, projs), counter')


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Expression walk — extract read accesses
-- ═══════════════════════════════════════════════════════════════════════

-- | Returns (reads, projections, updated counter).
extractReads
  :: forall ps decls n (d :: DomTag ps n) a.
     ReadCtx -> Int -> Expr ps decls n d a
  -> ([NamedMap], [NamedMap], Int)

extractReads _ c (Var _)   = ([], [], c)
extractReads _ c (Const _) = ([], [], c)

extractReads ctx c (Pw _ e1 e2) =
  let (r1, p1, c1) = extractReads ctx c  e1
      (r2, p2, c2) = extractReads ctx c1 e2
  in (r1 ++ r2, p1 ++ p2, c2)

extractReads ctx c (PMap _ e) = extractReads ctx c e

extractReads ctx c
  (Dep (Proxy :: Proxy mapCs)
       (inner :: Expr ps decls no dInner a)) =
  let ni   = rcNDims ctx
      no_  = fromIntegral (natVal (Proxy @no)) :: Int
      mapCs_ = reifySTConstraintsMapSplit ni
                 (knownConstraints @ps @(n + no) @mapCs)
      readMap = NamedMap
        { nmDomainName = Just (rcDomName ctx)
        , nmRangeName  = Just (innerVarName inner)
        , nmParams     = rcParams ctx
        , nmNIn        = ni
        , nmNOut       = no_
        , nmConjs      = [Conjunction (mapCs_ ++ mapDomToMap (rcDomCs ctx))]
        }
      innerCtx = ctx { rcDomCs = reflectDomConstraints @ps @no @dInner
                     , rcNDims = no_ }
      (innerReads, innerProjs, c') = extractReads innerCtx c inner
  in (readMap : innerReads, innerProjs, c')

extractReads ctx c
  (Reduce _reduceOp (Proxy :: Proxy projCs)
          (body :: Expr ps decls nBody dBody a)) =
  let nEq     = rcNDims ctx
      nBody_  = fromIntegral (natVal (Proxy @nBody)) :: Int
      bodyDomCs = reflectDomConstraints @ps @nBody @dBody
      bodyName = rcEqName ctx ++ "__body_" ++ show c
      c' = c + 1
      -- Projection: { bodyName[d0..d_{nBody-1}] → eqName[d0..d_{n-1}] }
      projMap = NamedMap
        { nmDomainName = Just bodyName
        , nmRangeName  = Just (rcEqName ctx)
        , nmParams     = rcParams ctx
        , nmNIn        = nBody_
        , nmNOut       = nEq
        , nmConjs      = [Conjunction
            (  [ EqualityConstraint
                   (C.Add (C.Ix (OutDim k)) (C.Mul (-1) (C.Ix (InDim k))))
               | k <- [0 .. nEq - 1] ]
            ++ mapDomToMap bodyDomCs
            )]
        }
      bodyCtx = ctx
        { rcDomCs   = bodyDomCs
        , rcDomName = bodyName
        , rcNDims   = nBody_
        }
      (bodyReads, bodyProjs, c'') = extractReads bodyCtx c' body
  in (bodyReads, projMap : bodyProjs, c'')

extractReads ctx c (Case branches) =
  extractBranchReads ctx c branches

extractBranchReads
  :: forall ps decls n (amb :: DomTag ps n) branchDoms a.
     ReadCtx -> Int -> Branches ps decls n amb branchDoms a
  -> ([NamedMap], [NamedMap], Int)
extractBranchReads _ c BNil = ([], [], c)
extractBranchReads ctx c (BCons (_ :: Proxy d) body rest) =
  let effDomCs = reflectDomConstraints @ps @n @(EffectiveDomTag d amb)
      branchCtx = ctx { rcDomCs = effDomCs }
      (bodyReads, bodyProjs, c1) = extractReads branchCtx c body
      (restReads, restProjs, c2) = extractBranchReads ctx c1 rest
  in (bodyReads ++ restReads, bodyProjs ++ restProjs, c2)


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Helpers
-- ═══════════════════════════════════════════════════════════════════════

innerVarName :: forall ps decls n (d :: DomTag ps n) a.
                Expr ps decls n d a -> String
innerVarName (Var (Proxy :: Proxy name)) = symbolVal (Proxy @name)
innerVarName (PMap _ e) = innerVarName e
innerVarName (Dep _ inner) = innerVarName inner
innerVarName _ = "<unknown>"

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
