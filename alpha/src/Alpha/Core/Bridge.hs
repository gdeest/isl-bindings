{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

-- | Legacy 'Alpha.Core' to 'Alpha.Core.V2' bridge.
--
-- Walks the list-index-fused GADT in "Alpha.Core", fresh-skolemizes
-- every declaration / domain / map, and rebuilds the equivalent
-- skolem-indexed 'Alpha.Core.V2.System'.  This is the sole adapter
-- allowing leaf-up migration of consumers from legacy Core to V2
-- during Phase B; deleted at Phase C when Surface is retargeted.
--
-- = Check vs fabricate
--
-- The bridge runs the V2 checkers ('checkImageSubset', 'checkPartition',
-- 'checkSubset') at every polyhedral node, even though the legacy GADT
-- has already proven the same obligation via the isl-plugin's
-- type-level dispatch.  We accept the ~1 ISL call per node cost as a
-- /double check/: it asserts that the token-level story matches the
-- type-level one, and would fire if "Alpha.Core.Tokens" ever drifted
-- from "Isl.TypeLevel.Reflection" invariants.
--
-- A faster variant that fabricates tokens with an unsafeCoerce-backed
-- 'mkToken' would be sound by the above argument; we keep the checks
-- here as long as elaboration is not a hotspot.  Trade can be
-- reconsidered after Phase B.3 (see plan §Risks).
--
-- = Trust boundary
--
-- Two 'unsafeCoerce's sit in this module:
--
--   * 'mkInScopeToken' — mints an 'InScope' phantom.  The token is
--     uninhabited and parameter-phantom; the coerce is operationally
--     inert.  Same pattern as "Alpha.Core.Tokens.mkToken".
--
--   * 'fakeDomEq' — coerces a freshly-minted expression from one
--     domain skolem to another when both were reflected from the
--     same legacy 'DomTag'.  Same pattern as
--     "Alpha.Core.Reify.fakeVarDom"; the legacy GADT's structural
--     type equality already witnesses that the two skolems index
--     the same ISL set.
module Alpha.Core.Bridge
  ( toV2
  , BridgeError
  ) where

import Data.Kind            (Type)
import Data.Proxy           (Proxy(..))
import Data.Type.Equality   ((:~:)(Refl))
import GHC.TypeLits         (KnownNat, natVal, symbolVal, type (+))
import Unsafe.Coerce        (unsafeCoerce)

import qualified Alpha.Core        as Legacy
import qualified Alpha.Core.V2     as V2
import           Alpha.Core.V2
  ( System(..)
  , SomeVarDecl(..)
  , SomeDefinesAll(..)
  , VarDecl(..)
  , CaseBranches(..)
  )
import           Alpha.Core.Named   (Named, name2)
import           Alpha.Core.Reify
  ( KnownDom, KnownVar
  , VarDomOf, ScalarTy
  , withFreshDom, withFreshMap, withFreshVar
  , reifyDom, reifyVar
  )
import           Alpha.Core.Tokens
  ( ElabError(..)
  , IslSet, IslMap
  , Partition, Subset, ImageSubset, InScope
  , checkImageSubset, checkPartition, checkSubset, checkDefinesAll
  )
import qualified Alpha.Scalar       as Scalar

import           Isl.Typed.Constraints
  ( NamedSet(..), NamedMap(..), Conjunction(..)
  , Constraint(..), MapIx(..)
  )
import Isl.Typed.Constraints (Expr(..)
  )
import           Isl.Typed.Params   (KnownSymbols, symbolVals)
import           Isl.TypeLevel.Reflection (DomTag, EffectiveDomTag, reflectDomConjunctions)
import           Isl.TypeLevel.Sing (knownConstraints, reifySTConstraintsMapSplit)
import qualified System.IO.Unsafe  as Unsafe


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Types
-- ═══════════════════════════════════════════════════════════════════════

-- | The bridge surfaces the same error shape as the elaborator.
type BridgeError = ElabError

-- | A variable reference: the var skolem @v@, its domain skolem @dom@
-- (constrained by @dom ~ VarDomOf sys v@), plus the 'InScope sys v'
-- token minted at installation time.
data SomeVarRef sys where
  SomeVarRef
    :: ( KnownVar sys v
       , KnownDom sys dom
       , dom ~ VarDomOf sys v )
    => Proxy v
    -> Proxy dom
    -> InScope sys v
    -> SomeVarRef sys

type VarEnv sys = [(String, SomeVarRef sys)]

-- ═══════════════════════════════════════════════════════════════════════
-- §2. Trust-boundary helpers
-- ═══════════════════════════════════════════════════════════════════════

-- | Mint an 'InScope sys v' token.  Phantom — runtime value is unit;
-- the relation "v is declared in sys" is witnessed by the caller
-- (only 'installDecls' below mints these tokens, and it does so
-- exactly when it has just installed a fresh 'KnownVar sys v').
mkInScopeToken :: forall sys v. InScope sys v
mkInScopeToken = unsafeCoerce ()

-- | Coerce an expression from one domain skolem to another when both
-- were reflected from the same legacy 'DomTag' at the same call site.
-- Operationally inert: 'V2.Expr' holds only proxies / tokens (all
-- phantom in their parameters), and both domain skolems reflect to
-- the same 'IslSet' by construction.
--
-- Same axiom category as "Alpha.Core.Reify.fakeVarDom"; documented
-- here as part of the single Alpha-bridge trust boundary.
fakeDomEq :: forall d1 d2. (d1 :~: d2)
fakeDomEq = unsafeCoerce (Refl :: () :~: ())

-- ═══════════════════════════════════════════════════════════════════════
-- §3. Public entry point
-- ═══════════════════════════════════════════════════════════════════════

-- | Bridge a legacy 'Legacy.System' to a fresh skolem-indexed
-- 'V2.System'.  The continuation is rank-N in @sys@.
--
-- The caller supplies the system's element type @a@ as an explicit
-- type argument.  Phase A's bridge does not verify that every
-- equation body's 'DeclType' matches @a@; the legacy GADT's
-- 'Defines' constructor already enforces that.
--
-- Failure short-circuits: the first checker to return 'Left'
-- propagates immediately without continuing the walk.
toV2
  :: forall ps pctx inputs outputs locals a r.
     KnownSymbols ps
  => Legacy.System ps pctx inputs outputs locals
  -> (forall sys. Either BridgeError (V2.System sys a) -> r)
  -> r
toV2 (Legacy.System (Legacy.Decls ins outs locs) eqs) k =
  name2 (symbolVals @ps) (emptyIslSet (symbolVals @ps)) $
    \(namedPs :: Named sys [String]) (namedPctx :: Named sys IslSet) ->
      installDecls @ps (Proxy @sys) [] ins       $ \env1 inEntries ->
      installDecls @ps (Proxy @sys) env1 outs    $ \env2 outEntries ->
      installDecls @ps (Proxy @sys) env2 locs    $ \env3 locEntries ->
        case bridgeEqList @ps @pctx @sys @_ @_ @a env3 eqs of
          Left err   -> k (Left err)
          Right bEqs ->
            let dcs  = map fst outEntries ++ map fst locEntries
                enms = map someEqName bEqs
            in case checkDefinesAll @'[] @'[] dcs enms of
                 Left err  -> k (Left err)
                 Right tot ->
                   k (Right System
                     { V2.sysParams    = namedPs
                     , V2.sysParamCs   = namedPctx
                     , V2.sysInputs    = map snd inEntries
                     , V2.sysOutputs   = map snd outEntries
                     , V2.sysLocals    = map snd locEntries
                     , V2.sysEqs       = bEqs
                     , V2.sysTotality  = SomeDefinesAll tot
                     })

-- | A placeholder empty parameter-context set.  The legacy 'pctx' is a
-- type-level list of constraints; Phase A materializes it as an empty
-- 'NamedSet' (no constraints on params).  Phase B adds reflection of
-- the type-level pctx into this set.
emptyIslSet :: [String] -> IslSet
emptyIslSet params = NamedSet
  { nsName   = Just "__pctx"
  , nsParams = params
  , nsNDims  = 0
  , nsConjs  = [Conjunction []]
  }

-- ═══════════════════════════════════════════════════════════════════════
-- §4. Declaration installation
-- ═══════════════════════════════════════════════════════════════════════

-- | Walk a legacy 'Legacy.DeclList', installing each declaration as
-- a fresh @(v, dom)@ skolem pair via 'withFreshVar'.  Threads the
-- growing 'VarEnv' forward and collects @(name, SomeVarDecl sys)@
-- for the system's input/output/local slots.
installDecls
  :: forall ps sys decls r.
     KnownSymbols ps
  => Proxy sys
  -> VarEnv sys
  -> Legacy.DeclList ps decls
  -> ( VarEnv sys -> [(String, SomeVarDecl sys)] -> r )
  -> r
installDecls _ env Legacy.Nil k = k env []
installDecls pSys env ((Legacy.MkDecl :: Legacy.Decl ps d) Legacy.:> rest) k =
  let nm     = symbolVal (Proxy @(Legacy.DeclName d))
      dims   = fromIntegral (natVal (Proxy @(Legacy.DeclDims d))) :: Int
      -- Phase A: scalar type is fixed to CFloat64.  See comment on
      -- 'anonScalarTy' below.
      scalar = anonScalarTy
      islSet = NamedSet
        { nsName   = Just nm
        , nsParams = symbolVals @ps
        , nsNDims  = dims
        , nsConjs  = reflectDomConjunctions
                       @ps @(Legacy.DeclDims d) @(Legacy.DeclDomTag d)
        }
  in withFreshVar pSys (nm, dims, scalar, islSet) $
       \(pv :: Proxy v_) (pd :: Proxy d_) ->
         let tok   = mkInScopeToken @sys @v_
             vref  = SomeVarRef pv pd tok
             vdecl = V2.VarDecl { vdName = nm, vdDims = dims, vdScalar = scalar }
             entry = (nm, SomeVarDecl pv vdecl)
         in installDecls @ps pSys ((nm, vref) : env) rest $ \env' entries ->
              k env' (entry : entries)

-- | Scalar-type placeholder.  The legacy 'VarDecl' kind carries the
-- element type @a@ in its 5th kind-application, but there is no
-- runtime 'AlphaScalar' dict projected out by 'MkDecl' today.  Phase
-- B adds a 'KnownScalar' constraint to 'MkDecl' that emits the right
-- 'CNumType'; for A-phase scaffolding we install 'CFloat64' as a
-- conservative default that matches the majority of test examples.
-- Consumers of 'V2.vdScalar' must not rely on this during Phase A.
anonScalarTy :: ScalarTy
anonScalarTy = Scalar.CFloat64

-- ═══════════════════════════════════════════════════════════════════════
-- §5. Equation-list bridge
-- ═══════════════════════════════════════════════════════════════════════

bridgeEqList
  :: forall ps pctx sys decls defined a.
     KnownSymbols ps
  => VarEnv sys
  -> Legacy.EqList ps pctx decls defined
  -> Either BridgeError [V2.SomeEquation sys a]
bridgeEqList _ Legacy.EqNil = Right []
bridgeEqList env (Legacy.Defines (Proxy :: Proxy nm) body Legacy.:& rest) = do
  let nmS = symbolVal (Proxy @nm)
  case lookup nmS env of
    Nothing -> Left (MissingDef nmS)
    Just (SomeVarRef (pv :: Proxy v) (pd :: Proxy dom) _tok) ->
      -- Phase A scalar monomorphism: the caller's @a@ is trusted to
      -- match every equation body's 'DeclType'.  The legacy GADT's
      -- 'Defines' constructor structurally forces the body's scalar
      -- to be 'DeclType (Lookup nm decls)'; V2's 'SomeEquation sys a'
      -- wants a single @a@.  Close the gap with 'coerceScalar' — same
      -- axiom category as 'fakeDomEq'.  Phase B replaces this with
      -- per-equation scalar existentials inside 'SomeEquation'.
      case coerceScalar body of
        eBodyA -> do
          eBody <- bridgeExprAt @ps @pctx @sys @dom pd env eBodyA
          es    <- bridgeEqList @ps @pctx @sys @_ @_ @a env rest
          pure (V2.SomeEquation pv eBody : es)
  where
    coerceScalar :: forall n_ d_ b.
                    Legacy.Expr ps pctx decls n_ d_ b
                 -> Legacy.Expr ps pctx decls n_ d_ a
    coerceScalar = unsafeCoerce

-- | Recover a 'SomeEquation''s defined-var name without threading the
-- dict explicitly; pattern-matching 'SomeEquation' brings 'KnownVar'
-- into scope so 'reifyVar' resolves.
someEqName :: forall sys a. V2.SomeEquation sys a -> String
someEqName (V2.SomeEquation (pv :: Proxy v) _) =
  let (nm, _, _) = reifyVar (Proxy @sys) pv
  in nm

-- ═══════════════════════════════════════════════════════════════════════
-- §6. Expression bridge
-- ═══════════════════════════════════════════════════════════════════════

-- | Bridge a legacy expression whose output domain is already known
-- (by skolem).  The caller provides the V2 @dom@ skolem; bridgeExprAt
-- propagates it through structural nodes (Pw, PMap, Const) and
-- installs fresh skolems for domain-shifting nodes (Dep, Reduce, Case).
--
-- Direct-style (not CPS) because structural pointwise nodes need to
-- reuse the caller's @dom@ across both operands; a CPS signature
-- would produce two distinct skolems.  The tradeoff is that
-- Dep/Reduce/Case use 'withFreshDom' / 'withFreshMap' locally,
-- producing V2 nodes whose internal skolems are opaque from outside.
bridgeExprAt
  :: forall ps pctx sys dom a n d decls.
     ( KnownSymbols ps
     , KnownDom sys dom )
  => Proxy dom
  -> VarEnv sys
  -> Legacy.Expr ps pctx decls n d a
  -> Either BridgeError (V2.Expr sys dom a)
bridgeExprAt _ env (Legacy.Var (Proxy :: Proxy nm)) =
  case lookup (symbolVal (Proxy @nm)) env of
    Nothing -> Left (MissingDef (symbolVal (Proxy @nm)))
    Just (SomeVarRef (pv :: Proxy v) _pd tok) ->
      -- The legacy GADT's 'Var' produces an 'Expr' whose domain is
      -- @DeclDomTag (Lookup nm decls)@.  bridgeExprAt's caller has
      -- already arranged for the V2 @dom@ skolem to reflect from the
      -- same DomTag (either directly, at the equation root, or via a
      -- Dep/Reduce/Case ancestor that installed it).  The equality
      -- @dom ~ VarDomOf sys v@ is therefore true by reflection
      -- structure; we close it with 'fakeDomEq'.  Same axiom category
      -- as 'Alpha.Core.Reify.fakeVarDom'.
      case fakeDomEq @dom @(VarDomOf sys v) of
        Refl -> Right (V2.Var tok pv)

bridgeExprAt _ _ (Legacy.Const c) =
  Right (V2.Const c)

bridgeExprAt pDom env (Legacy.Pw op e1 e2) = do
  b1 <- bridgeExprAt @ps @pctx @sys @dom pDom env e1
  b2 <- bridgeExprAt @ps @pctx @sys @dom pDom env e2
  pure (V2.Pw op b1 b2)

bridgeExprAt pDom env (Legacy.PMap op e) = do
  b <- bridgeExprAt @ps @pctx @sys @dom pDom env e
  pure (V2.PMap op b)

bridgeExprAt _pDom env
  (Legacy.Dep (Proxy :: Proxy mapCs)
              (body :: Legacy.Expr ps pctx decls no dInner a)) =
  let ni        = fromIntegral (natVal (Proxy @n)) :: Int
      nInner    = fromIntegral (natVal (Proxy @no)) :: Int
      srcConjs  = reflectDomConjunctions @ps @no @dInner
      srcSet    = NamedSet
                    { nsName   = Nothing
                    , nsParams = symbolVals @ps
                    , nsNDims  = nInner
                    , nsConjs  = srcConjs
                    }
      -- Legacy semantic: map goes outer (ni, result) → inner (no, array).
      -- V2 Dep's 'ImageSubset m src dom' token expects
      --   image(m | src) ⊆ dom,
      -- with body = src (= inner) and result = dom (= outer).
      -- That requires @m@ to go inner → outer, i.e., the *inverse* of
      -- the legacy access map.  We invert by swapping 'InDim k' ↔
      -- 'OutDim k' in every reified constraint and swapping nIn/nOut;
      -- the resulting NamedMap encodes the inverse relation with the
      -- same constraint text, correct for V2's convention.
      legacyMapCs = reifySTConstraintsMapSplit ni
                      (knownConstraints @ps @(n + no) @mapCs)
      mapConjs    = [Conjunction (map (invertMapConstraint ni nInner) legacyMapCs)]
      depMap      = NamedMap
                    { nmDomainName = Nothing
                    , nmRangeName  = Nothing
                    , nmParams     = symbolVals @ps
                    , nmNIn        = nInner    -- inner (source array)
                    , nmNOut       = ni        -- outer (iteration)
                    , nmConjs      = mapConjs
                    }
  in withFreshDom (Proxy @sys) srcSet $ \(pSrc :: Proxy src) ->
       withFreshMap (Proxy @sys) depMap $ \(pM :: Proxy m) ->
         -- Phase A double-check: run the V2 ISL checker as an
         -- assertion that the legacy plugin's obligation is also
         -- ISL-valid.
         case runCheckImageSubset @m @src @dom depMap srcSet (theDom @sys @dom) of
           Left err  -> Left err
           Right tok -> do
             bodyE <- bridgeExprAt @ps @pctx @sys @src pSrc env body
             pure (V2.Dep tok pM bodyE)

bridgeExprAt _pDom env
  (Legacy.Reduce op (Proxy :: Proxy projCs)
                (body :: Legacy.Expr ps pctx decls nBody dBody a)) =
  let n        = fromIntegral (natVal (Proxy @n)) :: Int
      nB       = fromIntegral (natVal (Proxy @nBody)) :: Int
      bodyConjs = reflectDomConjunctions @ps @nBody @dBody
      bodySet   = NamedSet
                    { nsName   = Nothing
                    , nsParams = symbolVals @ps
                    , nsNDims  = nB
                    , nsConjs  = bodyConjs
                    }
      -- Projection map: input dims 0..nBody-1, output dims nBody..nBody+n-1.
      projConjs = [Conjunction
                     (reifySTConstraintsMapSplit nB
                        (knownConstraints @ps @(nBody + n) @projCs))]
      projMap   = NamedMap
                    { nmDomainName = Nothing
                    , nmRangeName  = Nothing
                    , nmParams     = symbolVals @ps
                    , nmNIn        = nB
                    , nmNOut       = n
                    , nmConjs      = projConjs
                    }
  in withFreshDom (Proxy @sys) bodySet $ \(pBody :: Proxy bodyD) ->
       withFreshMap (Proxy @sys) projMap $ \(pP :: Proxy p) ->
         case runCheckImageSubset @p @bodyD @dom projMap bodySet (theDom @sys @dom) of
           Left err  -> Left err
           Right tok -> do
             bodyE <- bridgeExprAt @ps @pctx @sys @bodyD pBody env body
             pure (V2.Reduce tok op pP bodyE)

bridgeExprAt pDom env (Legacy.Case branches) =
  bridgeBranches @ps @pctx @sys @dom @_ pDom env branches
    (theDom @sys @dom)

-- | Recover the 'IslSet' for a @KnownDom sys dom@ via the class dict,
-- stripped of its tuple name.  ISL compares sets by their full space
-- (including tuple names); two sets with the same conjunction but
-- different tuple names are /not/ equal under 'isSubset'.  The V2
-- tokens are skolem-identified so tuple names carry no identity —
-- strip them before checker calls so the ISL subset/equal tests
-- focus on the conjunction content.
theDom :: forall sys dom. KnownDom sys dom => IslSet
theDom = stripTupleName (reifyDom (Proxy @sys) (Proxy @dom))
  where
    stripTupleName ns = ns { nsName = Nothing }

-- ═══════════════════════════════════════════════════════════════════════
-- §7. Case branches
-- ═══════════════════════════════════════════════════════════════════════

-- | Walk a legacy 'Legacy.Branches' snoc-list, installing a fresh
-- branch-domain skolem per entry and collecting the branch 'IslSet's
-- for the final 'checkPartition' call.  Returns a V2 'Case'
-- expression on success.
--
-- The partition check runs inside 'collectBranches'' continuation so
-- that the @bs@ type-level skolem list (closed over by
-- 'collectBranches'' rank-N callback) is in scope for the
-- 'Partition dom bs' token.
bridgeBranches
  :: forall ps pctx sys dom n (amb :: DomTag ps n) a branchDoms decls.
     ( KnownSymbols ps
     , KnownDom sys dom
     , KnownNat n )
  => Proxy dom
  -> VarEnv sys
  -> Legacy.Branches ps pctx decls n amb branchDoms a
  -> IslSet                -- ambient ISL set (for partition check)
  -> Either BridgeError (V2.Expr sys dom a)
bridgeBranches pDom env branches ambSet =
  collectBranches @ps @pctx @sys @dom @n @amb pDom env branches $
    \(bs :: V2.CaseBranches sys bs_ a) brSets ->
      case runCheckPartition @dom @bs_ ambSet brSets of
        Left err  -> Left err
        Right tok -> Right (V2.Case tok bs)

-- | CPS walker over 'Legacy.Branches' that accumulates V2 'CaseBranches'
-- and a parallel list of 'IslSet' branch domains for the partition
-- check.  The rank-N continuation closes over the '[Type]' index of
-- 'CaseBranches'.
collectBranches
  :: forall ps pctx sys dom n (amb :: DomTag ps n) branchDoms decls a.
     ( KnownSymbols ps
     , KnownDom sys dom
     , KnownNat n )
  => Proxy dom
  -> VarEnv sys
  -> Legacy.Branches ps pctx decls n amb branchDoms a
  -> (forall (bs :: [Type]). V2.CaseBranches sys bs a -> [IslSet]
      -> Either BridgeError (V2.Expr sys dom a))
  -> Either BridgeError (V2.Expr sys dom a)
collectBranches _ _ Legacy.BNil k =
  k BNil []
collectBranches pDom env
  (Legacy.BCons (Proxy :: Proxy d_i)
                (body :: Legacy.Expr ps pctx decls n (EffectiveDomTag d_i amb) a)
                rest) k =
  let brConjs = reflectDomConjunctions @ps @n @(EffectiveDomTag d_i amb)
      brSet   = NamedSet
                  { nsName   = Nothing
                  , nsParams = symbolVals @ps
                  , nsNDims  = fromIntegral (natVal (Proxy @n))
                  , nsConjs  = brConjs
                  }
  in withFreshDom (Proxy @sys) brSet $ \(pB :: Proxy b) ->
       case runCheckSubset @b @dom brSet (theDom @sys @dom) of
         Left err  -> Left err
         Right tok ->
           case bridgeExprAt @ps @pctx @sys @b pB env body of
             Left err     -> Left err
             Right bodyE  ->
               collectBranches @ps @pctx @sys @dom @n @amb pDom env rest $
                 \tailBs tailSets ->
                   k (BCons tok pB bodyE tailBs) (brSet : tailSets)

-- ═══════════════════════════════════════════════════════════════════════
-- §8. Synchronous checker wrappers
-- ═══════════════════════════════════════════════════════════════════════
--
-- The token checkers in "Alpha.Core.Tokens" return 'IO (Either ...)'
-- to keep ISL calls in IO.  The checkers already internally
-- 'unsafePerformIO . runIslT' before returning, so IO is a newtype
-- wrapper around a pure computation; these helpers unwrap it.

runCheckImageSubset
  :: forall m src tgt.
     IslMap -> IslSet -> IslSet
  -> Either BridgeError (ImageSubset m src tgt)
runCheckImageSubset mm src tgt =
  Unsafe.unsafePerformIO (checkImageSubset @m @src @tgt mm src tgt)
{-# NOINLINE runCheckImageSubset #-}

runCheckPartition
  :: forall amb bs.
     IslSet -> [IslSet]
  -> Either BridgeError (Partition amb bs)
runCheckPartition amb bsL =
  Unsafe.unsafePerformIO (checkPartition @amb @bs amb bsL)
{-# NOINLINE runCheckPartition #-}

runCheckSubset
  :: forall a b.
     IslSet -> IslSet
  -> Either BridgeError (Subset a b)
runCheckSubset a b =
  Unsafe.unsafePerformIO (checkSubset @a @b a b)
{-# NOINLINE runCheckSubset #-}

-- ═══════════════════════════════════════════════════════════════════════
-- §9. Map-index inversion
-- ═══════════════════════════════════════════════════════════════════════

-- | Swap 'InDim' and 'OutDim' in a single 'MapIx'-indexed constraint,
-- producing the constraint text for the inverse relation.  Used by
-- the Dep bridge to align legacy (outer → inner) maps with V2's
-- (inner → outer) convention.
--
-- The @_ni@ and @_no@ arguments are unused at the value level but
-- kept for documentation: the resulting constraint lives in a
-- 'MapIx' space where nIn and nOut are swapped relative to the
-- input.
invertMapConstraint :: Int -> Int -> Constraint MapIx -> Constraint MapIx
invertMapConstraint _ _ (EqualityConstraint   e) = EqualityConstraint   (invertMapExpr e)
invertMapConstraint _ _ (InequalityConstraint e) = InequalityConstraint (invertMapExpr e)

invertMapExpr :: Expr MapIx -> Expr MapIx
invertMapExpr = go
  where
    go (Ix mi)         = Ix (invertMapIx mi)
    go (Constant c)    = Constant c
    go (Add a b)       = Add (go a) (go b)
    go (Mul k a)       = Mul k (go a)
    go (FloorDiv a d)  = FloorDiv (go a) d

invertMapIx :: MapIx -> MapIx
invertMapIx (InDim k)     = OutDim k
invertMapIx (OutDim k)    = InDim k
invertMapIx (MapParam k)  = MapParam k
