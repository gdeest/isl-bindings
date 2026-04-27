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
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}

-- | Surface-to-Core elaboration: cash in plugin dicts, mint GDP tokens.
--
-- Walks a plugin-typed 'Alpha.Surface.Core.System' and produces a
-- GDP-indexed 'Alpha.Core.System' by calling the 'tokenize*' helpers
-- in 'Alpha.Core.Tokens' at each polyhedral node.
--
-- = Trust boundary
--
-- The elaborator is the sole client of the 'tokenize*' / @axiom*@
-- family from 'Alpha.Core.Tokens'.  Each call site cashes in a
-- plugin-discharged obligation in the current constraint scope.  No
-- 'unsafeCoerce' appears in this module; all trust goes through
-- 'Alpha.Core.Tokens.mkToken'.
--
-- = Dep polarity
--
-- 'Alpha.Core.Dep's token is @ImageSubset m dom src@: the access map
-- @m : dom → src@ sends iteration points into the accessed array's
-- declared domain.  Surface's 'IslImageSubsetD' plugin dict proves
-- @image(mapCs | dOuter) ⊆ dInner@ — binding dOuter→dom and
-- dInner→src makes the two witnesses structurally identical, so the
-- elaborator can cash in with no ISL work in 'TrustPlugin' mode.
module Alpha.Surface.Elaborate
  ( elaborate
  , ElabMode(..)
  , ElabError
  ) where

import Data.Kind             (Type)
import Data.Proxy            (Proxy(..))
import Data.Type.Equality    ((:~:)(Refl))
import GHC.TypeLits
  ( KnownNat, KnownSymbol, natVal, symbolVal, type (+) )
import System.IO.Unsafe      (unsafePerformIO)

import qualified Alpha.Core            as Core
import qualified Alpha.Surface.Core    as Surface
import           Alpha.Core.Named       (Named, name, name2)
import           Alpha.Core.Tokens
  ( ElabError(..)
  , ElabMode(..)
  , InScope
  , IslMap
  , IslSet
  , Partition
  , SomeNamedSet(..)
  , axiomBranchSubset
  , axiomDomEq
  , axiomScalarEq
  , checkAndTokenizeImageEqual
  , checkAndTokenizeImageSubset
  , checkAndTokenizePartition
  , checkAndTokenizeSubset
  , checkDefinesAll
  , tokenizeInScope
  )
import qualified Alpha.Scalar          as Scalar

import           Isl.Typed.Constraints  (NamedMap(..), NamedSet(..), Conjunction(..))
import qualified Isl.Typed.Constraints as C
import           Isl.Typed.Params       (KnownSymbols, symbolVals)
import           Isl.TypeLevel.Constraint (LiftPctxN)
import           Isl.TypeLevel.Reflection
  ( Append, DomTag, EffectiveDomTag, IslImageEqualD, IslImageSubsetD, IslPartitionsD
  , IslSubsetD, KnownDom, LitPrepend, MapLitPrepend, reflectDomConjunctions )
import           Isl.TypeLevel.Sing
  ( KnownConstraints, knownConstraints
  , reifySTConstraintsMap, reifySTConstraintsMapSplit, reifySTConstraintsSet )


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Environment
-- ═══════════════════════════════════════════════════════════════════════

-- | A variable reference resolved during elaboration: the symbol name
-- at the type level, the 'InScope' token, the 'Named dom IslSet'
-- payload, and the 'Core.VarDecl' built at declaration time.  The
-- 'VarDecl' field is the single source of truth for vdScalar and
-- vdDims; 'walkEquations' threads it directly into 'SomeEquation'
-- rather than reconstructing.
data SomeVarRef sys where
  SomeVarRef
    :: forall sys v (dom :: Type).
       KnownSymbol v
    => Proxy v
    -> InScope sys v
    -> Named dom IslSet
    -> Core.VarDecl sys v dom
    -> SomeVarRef sys

-- | Symbol → reference lookup.
type VarEnv sys = [(String, SomeVarRef sys)]


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Public entry point
-- ═══════════════════════════════════════════════════════════════════════

-- | Elaborate a Surface system to a Core system, minting GDP tokens
-- by cashing in the plugin dicts attached to each Surface node.
--
-- The caller chooses the scalar @a@; every equation's @DeclType@ is
-- asserted to match @a@ via 'axiomScalarEq' — a hard precondition of
-- calling 'elaborate' polymorphically over surface systems whose
-- declared-variable scalars might differ.  Typical callers pick @a@ to
-- match the system's actual uniform scalar (e.g. @Double@ for matmul).
elaborate
  :: forall ps pctx inputs outputs locals a r.
     (KnownSymbols ps, Scalar.AlphaScalar a)
  => ElabMode
  -> Surface.System ps pctx inputs outputs locals
  -> (forall sys. Either ElabError (Core.System sys a) -> r)
  -> r
elaborate mode (Surface.System (Surface.Decls ins outs locs) eqs) k =
  name2 (symbolVals @ps) (paramIslSet @ps @pctx (symbolVals @ps)) $
    \(namedPs   :: Named sys [String])
     (namedPctx :: Named sys IslSet) ->
      installDecls @ps @pctx @sys [] ins  $ \env1 inEntries  ->
      installDecls @ps @pctx @sys env1 outs $ \env2 outEntries ->
      installDecls @ps @pctx @sys env2 locs $ \env3 locEntries ->
        case walkEquations @ps @pctx @sys @_ @_ @a mode env3 eqs of
          Left err   -> k (Left err)
          Right sEqs ->
            let dcNames = map fst outEntries ++ map fst locEntries
                eqNames = map someEqName sEqs
            in case checkDefinesAll @'[] @'[] dcNames eqNames of
                 Left  err -> k (Left err)
                 Right tot ->
                   k (Right Core.System
                     { Core.sysParams    = namedPs
                     , Core.sysParamCs   = namedPctx
                     , Core.sysInputs    = map snd inEntries
                     , Core.sysOutputs   = map snd outEntries
                     , Core.sysLocals    = map snd locEntries
                     , Core.sysEqs       = sEqs
                     , Core.sysTotality  = Core.SomeDefinesAll tot
                     })

-- | Materialise the system's parameter-context constraint list
-- @pctx :: [TConstraint ps 0]@ into a zero-dim 'IslSet' over @ps@.
--
-- Mirrors the pattern used for per-variable domains in 'installDecls'
-- (conjunction of reflected constraints) but at dimension 0: every
-- TConstraint in @pctx@ reduces to a predicate on the parameters
-- alone, and the resulting set lives in the @[ps] -> { [] : ... }@
-- space.
--
-- This is the SanityCheck hinge: re-checks downstream call
-- 'buildBasicSet' on maps/domains that are all parameter-indexed by
-- this @pctx@.  An empty parameter set would silently let ISL
-- re-verify obligations under a weaker parameter context than the
-- plugin used.
paramIslSet
  :: forall ps pctx.
     (KnownConstraints ps 0 pctx)
  => [String] -> IslSet
paramIslSet params = NamedSet
  { nsName   = Just "__pctx"
  , nsParams = params
  , nsNDims  = 0
  , nsConjs  = [Conjunction (pctxSetCs @ps @pctx)]
  }

-- | Reflect the type-level @pctx@ as parameter-only 'SetIx' constraints.
-- Since @pctx :: [TConstraint ps 0]@ never references dim indices, the
-- same reflected list is a valid conjunction in every dim-@n@ set space
-- (params are dim-count-independent in 'SetIx').  Folding this into
-- each per-node 'NamedSet' gives ISL the pctx under which the plugin
-- dispatched the obligation, so 'SanityCheck' re-checks under the
-- correct parameter context.
pctxSetCs
  :: forall ps pctx.
     (KnownConstraints ps 0 pctx)
  => [C.Constraint C.SetIx]
pctxSetCs = reifySTConstraintsSet (knownConstraints @ps @0 @pctx)

-- | 'MapIx' counterpart of 'pctxSetCs'.  Parameter references in
-- 'MapIx' are @MapParam i@, independent of the (nIn, nOut) split — so
-- folding this into every per-node 'NamedMap' is sound regardless of
-- the map's input/output dim partition.
pctxMapCs
  :: forall ps pctx.
     (KnownConstraints ps 0 pctx)
  => [C.Constraint C.MapIx]
pctxMapCs = reifySTConstraintsMap (knownConstraints @ps @0 @pctx)


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Declaration installation
-- ═══════════════════════════════════════════════════════════════════════

-- | Walk a surface 'Surface.DeclList', installing each declared
-- variable into the environment with a fresh domain skolem minted via
-- 'name'.  Accumulates 'SomeVarDecl sys' for the outer 'Core.System'
-- slot-lists and 'SomeVarRef sys' entries for the body walker.
installDecls
  :: forall ps pctx sys decls r.
     (KnownSymbols ps, KnownConstraints ps 0 pctx)
  => VarEnv sys
  -> Surface.DeclList ps decls
  -> ( VarEnv sys -> [(String, Core.SomeVarDecl sys)] -> r )
  -> r
installDecls env Surface.Nil k = k env []
installDecls env ((Surface.MkDecl :: Surface.Decl ps d) Surface.:> rest) k =
  let nm   = symbolVal (Proxy @(Surface.DeclName d))
      dims = fromIntegral (natVal (Proxy @(Surface.DeclDims d))) :: Int
      -- Surface.MkDecl carries AlphaScalar (DeclType d) in its context,
      -- so pattern-matching brings the dict into scope and we can
      -- reflect the declared scalar to its runtime 'CNumType' tag.
      scalar  = Scalar.scalarCNumType @(Surface.DeclType d)
      -- Fold pctx (parameter-only) into the domain conjunction: every
      -- declared variable lives under the system's parameter context,
      -- so the domain ISL set should reflect that.  The pctx encoding
      -- uses only 'SetParam' references, independent of @dims@.
      domConjs = reflectDomConjunctions
                   @ps @(Surface.DeclDims d) @(Surface.DeclDomTag d)
      conjs  = fmap (foldPctxIntoSet (pctxSetCs @ps @pctx)) domConjs
      islSet = NamedSet
        { nsName   = Just nm
        , nsParams = symbolVals @ps
        , nsNDims  = dims
        , nsConjs  = conjs
        }
  in name islSet $ \(namedDom :: Named dom IslSet) ->
       let pv    = Proxy @(Surface.DeclName d)
           tok   = tokenizeInScope @sys pv
           vdecl = Core.VarDecl
                     { Core.vdName   = nm
                     , Core.vdDims   = dims
                     , Core.vdScalar = scalar
                     , Core.vdDom    = namedDom
                     }
           vref  = SomeVarRef pv tok namedDom vdecl
           entry = (nm, Core.SomeVarDecl pv vdecl)
       in installDecls @ps @pctx @sys ((nm, vref) : env) rest $ \env' entries ->
            k env' (entry : entries)

-- | Prepend parameter-only pctx constraints onto a set conjunction.
foldPctxIntoSet :: [C.Constraint C.SetIx] -> Conjunction C.SetIx -> Conjunction C.SetIx
foldPctxIntoSet px (Conjunction cs) = Conjunction (px ++ cs)

-- | Prepend parameter-only pctx constraints onto a map conjunction.
foldPctxIntoMap :: [C.Constraint C.MapIx] -> Conjunction C.MapIx -> Conjunction C.MapIx
foldPctxIntoMap px (Conjunction cs) = Conjunction (px ++ cs)


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Equation walker
-- ═══════════════════════════════════════════════════════════════════════

-- | Walk the surface 'Surface.EqList' producing a list of Core
-- 'SomeEquation sys a'.  For each equation: look up the declared
-- variable (to recover its VarDecl + Named domain), walk the body
-- against that domain, and pack with the declaration.
walkEquations
  :: forall ps pctx sys decls defined a.
     ( KnownSymbols ps
     , KnownConstraints ps 0 pctx
     , Scalar.AlphaScalar a )
  => ElabMode
  -> VarEnv sys
  -> Surface.EqList ps pctx decls defined
  -> Either ElabError [Core.SomeEquation sys a]
walkEquations _    _   Surface.EqNil = Right []
walkEquations mode env (Surface.Defines (Proxy :: Proxy nm) body Surface.:& rest) =
  let nmS = symbolVal (Proxy @nm)
  in case lookup nmS env of
       Nothing -> Left (MissingDef nmS)
       Just (SomeVarRef (pv :: Proxy v) tok (namedDom :: Named dom IslSet)
                        (vdecl :: Core.VarDecl sys v dom)) -> do
         bodyE <- walkExprAt @ps @pctx @sys @dom mode namedDom env (coerceBody body)
         let _ = tok
             _ = pv
         eqs <- walkEquations @ps @pctx @sys @decls @_ @a mode env rest
         pure (Core.SomeEquation vdecl bodyE : eqs)
  where
    coerceBody
      :: forall n_ d_ b.
         Surface.Expr ps pctx decls n_ d_ b
      -> Surface.Expr ps pctx decls n_ d_ a
    coerceBody e = case axiomScalarEq @b @a of Refl -> e

-- | Recover the defined-var name from a 'Core.SomeEquation'.
someEqName :: forall sys a. Core.SomeEquation sys a -> String
someEqName (Core.SomeEquation (Core.VarDecl { Core.vdName = n }) _) = n


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Expression walker
-- ═══════════════════════════════════════════════════════════════════════

-- | Walk a surface expression whose output domain skolem is known by
-- the caller.  Direct-style (not CPS) because Pw/PMap need both
-- operands on the same domain; Dep/Reduce/Case install fresh skolems
-- internally and re-enter 'walkExprAt' with the new ambient.
walkExprAt
  :: forall ps pctx sys dom a n d decls.
     ( KnownSymbols ps
     , KnownConstraints ps 0 pctx )
  => ElabMode
  -> Named dom IslSet
  -> VarEnv sys
  -> Surface.Expr ps pctx decls n d a
  -> Either ElabError (Core.Expr sys dom a)
walkExprAt _mode namedDom env (Surface.Var (Proxy :: Proxy nm)) =
  case lookup (symbolVal (Proxy @nm)) env of
    Nothing -> Left (MissingDef (symbolVal (Proxy @nm)))
    Just (SomeVarRef (pv :: Proxy v) tok (namedVarDom :: Named varDom IslSet) _vdecl) ->
      -- Surface 'Var' produces an expr on the variable's /declared/
      -- domain; Core 'Var' does the same.  The enclosing context has
      -- installed a fresh 'dom' skolem whose IslSet coincides with the
      -- variable's declared domain (the surface typing guarantees
      -- structural equality).  Bridge via 'axiomDomEq'.
      case axiomDomEq namedVarDom namedDom of
        Refl -> Right (Core.Var pv tok namedVarDom)

walkExprAt _mode _ _ (Surface.Const c) =
  Right (Core.Const c)

walkExprAt mode namedDom env (Surface.Pw op e1 e2) = do
  b1 <- walkExprAt @ps @pctx @sys @dom mode namedDom env e1
  b2 <- walkExprAt @ps @pctx @sys @dom mode namedDom env e2
  pure (Core.Pw op b1 b2)

walkExprAt mode namedDom env (Surface.PMap op e) = do
  b <- walkExprAt @ps @pctx @sys @dom mode namedDom env e
  pure (Core.PMap op b)

walkExprAt mode namedDom env
  (Surface.Dep (Proxy :: Proxy mapCs)
               (inner :: Surface.Expr ps pctx decls no dInner a)) =
  walkDep @ps @pctx @sys @dom @n @d @no @mapCs @dInner @a @decls
          mode namedDom env inner

walkExprAt mode namedDom env
  (Surface.Reduce op (Proxy :: Proxy projCs)
                  (body :: Surface.Expr ps pctx decls nBody dBody a)) =
  walkReduce @ps @pctx @sys @dom @n @d @nBody @projCs @dBody @a @decls
             mode namedDom env op body

walkExprAt mode namedDom env (Surface.Case branches) =
  walkCase @ps @pctx @sys @dom @n @d @_ @a @decls
           mode namedDom env branches

walkExprAt mode namedDom env
  (Surface.Restrict (inner :: Surface.Expr ps pctx decls n dInner a)) =
  walkRestrict @ps @pctx @sys @dom @n @d @dInner @a @decls
               mode namedDom env inner


-- ═══════════════════════════════════════════════════════════════════════
-- §6. Dep walker
-- ═══════════════════════════════════════════════════════════════════════
--
-- Cashes in the Surface 'Dep' constructor's 'IslImageSubsetD' dict:
-- @image(mapCs | dOuter) ⊆ dInner@ — binding Surface's @dOuter@ →
-- Core's @dom@ (iteration) and Surface's @dInner@ → Core's @src@
-- (array) yields the Core token @ImageSubset m dom src@ directly.

walkDep
  :: forall ps pctx sys dom ni dOuter no mapCs dInner a decls.
     ( KnownSymbols ps
     , KnownConstraints ps 0 pctx
     , KnownNat ni, KnownNat no
     , KnownDom ps ni dOuter
     , KnownDom ps no dInner
     , KnownConstraints ps (ni + no) mapCs
     , IslImageSubsetD ps ni no
         (Append (LiftPctxN (ni + no) pctx) mapCs)
         (LitPrepend (LiftPctxN ni pctx) dOuter)
         (LitPrepend (LiftPctxN no pctx) dInner) )
  => ElabMode
  -> Named dom IslSet
  -> VarEnv sys
  -> Surface.Expr ps pctx decls no dInner a
  -> Either ElabError (Core.Expr sys dom a)
walkDep mode namedDom env inner =
  let nIn  = fromIntegral (natVal (Proxy @ni)) :: Int
      nOut = fromIntegral (natVal (Proxy @no)) :: Int
      -- 'dInner' is the accessed array's declared domain.  Fold pctx
      -- into the set/map conjunctions so 'SanityCheck' re-runs ISL
      -- under the same parameter context the plugin used.
      srcConjs = fmap (foldPctxIntoSet (pctxSetCs @ps @pctx))
                   (reflectDomConjunctions @ps @no @dInner)
      srcSet   = NamedSet
        { nsName   = Nothing
        , nsParams = symbolVals @ps
        , nsNDims  = nOut
        , nsConjs  = srcConjs
        }
      -- Map constraint list: Surface's @mapCs@ with dims 0..ni-1 as
      -- input (iteration / outer) and ni..ni+no-1 as output (array
      -- / inner).  Matches Core.Dep polarity (@m : dom → src@, so
      -- nIn=ni, nOut=no).
      mapConjs = [foldPctxIntoMap (pctxMapCs @ps @pctx)
                    (Conjunction
                       (reifySTConstraintsMapSplit nIn
                         (knownConstraints @ps @(ni + no) @mapCs)))]
      depMap   = NamedMap
        { nmDomainName = Nothing
        , nmRangeName  = Nothing
        , nmParams     = symbolVals @ps
        , nmNIn        = nIn
        , nmNOut       = nOut
        , nmConjs      = mapConjs
        }
  in name srcSet $ \(namedSrc :: Named src IslSet) ->
     name depMap $ \(namedM :: Named m IslMap) ->
       case unsafePerformIO
              (checkAndTokenizeImageSubset
                 @ps @pctx @ni @no @mapCs @dOuter @dInner
                 @m @dom @src
                 mode
                 (Proxy @pctx) (Proxy @mapCs) (Proxy @dOuter) (Proxy @dInner)
                 namedM namedDom namedSrc) of
         Left err  -> Left err
         Right tok -> do
           innerE <- walkExprAt @ps @pctx @sys @src mode namedSrc env inner
           pure (Core.Dep namedM namedSrc tok innerE)


-- ═══════════════════════════════════════════════════════════════════════
-- §7. Reduce walker
-- ═══════════════════════════════════════════════════════════════════════
--
-- Cashes in Surface 'Reduce''s 'IslImageEqualD' dict:
-- @image(projCs | dBody) = d@.  Binding dBody → body (Core) and d →
-- dom (Core) gives Core's 'ImageEqual p body dom'.

walkReduce
  :: forall ps pctx sys dom n d nBody projCs dBody a decls.
     ( KnownSymbols ps
     , KnownConstraints ps 0 pctx
     , KnownNat n, KnownNat nBody
     , KnownDom ps n d
     , KnownDom ps nBody dBody
     , KnownConstraints ps (nBody + n) projCs
     , IslImageEqualD ps nBody n
         (Append (LiftPctxN (nBody + n) pctx) projCs)
         (LitPrepend (LiftPctxN nBody pctx) dBody)
         (LitPrepend (LiftPctxN n pctx) d) )
  => ElabMode
  -> Named dom IslSet
  -> VarEnv sys
  -> Surface.ReduceOp
  -> Surface.Expr ps pctx decls nBody dBody a
  -> Either ElabError (Core.Expr sys dom a)
walkReduce mode namedDom env op bodyE0 =
  let nR   = fromIntegral (natVal (Proxy @n))     :: Int
      nB   = fromIntegral (natVal (Proxy @nBody)) :: Int
      -- Fold pctx into body / projection sets for SanityCheck parity.
      bodyConjs = fmap (foldPctxIntoSet (pctxSetCs @ps @pctx))
                    (reflectDomConjunctions @ps @nBody @dBody)
      bodySet   = NamedSet
        { nsName   = Nothing
        , nsParams = symbolVals @ps
        , nsNDims  = nB
        , nsConjs  = bodyConjs
        }
      -- Projection map: input nBody dims, output n dims.
      projConjs = [foldPctxIntoMap (pctxMapCs @ps @pctx)
                     (Conjunction
                        (reifySTConstraintsMapSplit nB
                          (knownConstraints @ps @(nBody + n) @projCs)))]
      projMap   = NamedMap
        { nmDomainName = Nothing
        , nmRangeName  = Nothing
        , nmParams     = symbolVals @ps
        , nmNIn        = nB
        , nmNOut       = nR
        , nmConjs      = projConjs
        }
  in name bodySet $ \(namedBody :: Named bodyDom IslSet) ->
     name projMap $ \(namedP :: Named p IslMap) ->
       case unsafePerformIO
              (checkAndTokenizeImageEqual
                 @ps @pctx @nBody @n @projCs @dBody @d
                 @p @bodyDom @dom
                 mode
                 (Proxy @pctx) (Proxy @projCs) (Proxy @dBody) (Proxy @d)
                 namedP namedBody namedDom) of
         Left err  -> Left err
         Right tok -> do
           bodyE <- walkExprAt @ps @pctx @sys @bodyDom mode namedBody env bodyE0
           pure (Core.Reduce op namedP namedBody tok bodyE)


-- ═══════════════════════════════════════════════════════════════════════
-- §8. Case walker
-- ═══════════════════════════════════════════════════════════════════════
--
-- Walks the Surface branch list bottom-up, materializing each
-- branch's effective domain (d ∩ amb) via 'reflectDomConjunctions'
-- on 'EffectiveDomTag d amb', minting a fresh skolem per branch, and
-- recursively walking the body.  The 'Partition' token is cashed in
-- via 'tokenizePartition' / 'checkAndTokenizePartition'; each
-- branch's 'Subset b dom' token is minted from the 'Partition' via
-- 'axiomBranchSubset' (a logical consequence, no ISL call).

walkCase
  :: forall ps pctx sys dom n (amb :: DomTag ps n) branchDoms a decls.
     ( KnownSymbols ps
     , KnownConstraints ps 0 pctx
     , KnownNat n
     , KnownDom ps n amb
     , IslPartitionsD ps n
         (LitPrepend (LiftPctxN n pctx) amb)
         (MapLitPrepend (LiftPctxN n pctx) branchDoms) )
  => ElabMode
  -> Named dom IslSet
  -> VarEnv sys
  -> Surface.Branches ps pctx decls n amb branchDoms a
  -> Either ElabError (Core.Expr sys dom a)
walkCase mode namedDom env branches =
  -- Two-pass approach:
  --   Pass 1 (collectBranches) installs a fresh skolem per branch,
  --           walks each body, and yields the list of 'SomeNamedSet'
  --           required by 'checkAndTokenizePartition'.
  --   Pass 2 (attachSubsets) rebuilds the 'CaseBranches' chain under
  --           the now-in-scope 'Partition' token, minting each branch's
  --           'Subset b dom' via 'axiomBranchSubset'.
  -- The passes share the skolem list @bs@ via the rank-N continuation.
  collectBranches @ps @pctx @sys @dom @n @amb mode namedDom env branches $
    \(preCB  :: PreCaseBranches sys dom bs a)
     (someBranchSets :: [SomeNamedSet]) ->
      case unsafePerformIO
             (checkAndTokenizePartition
                @ps @pctx @n @amb @branchDoms
                @dom @bs
                mode
                (Proxy @pctx) (Proxy @amb) (Proxy @branchDoms)
                namedDom someBranchSets) of
        Left err      -> Left err
        Right partTok ->
          Right (Core.Case partTok
                   (attachSubsets @sys @dom @bs @bs @a partTok namedDom preCB))


-- | A 'CaseBranches'-shaped chain that defers its per-branch
-- 'Subset b dom' tokens.  We can't build a real 'Core.CaseBranches'
-- inside 'collectBranches' because the 'Partition dom bs' token
-- isn't yet in scope; instead we accumulate this structural mirror
-- and paste the Subset tokens on in a second pass.
type PreCaseBranches :: Type -> Type -> [Type] -> Type -> Type
data PreCaseBranches sys dom bs a where
  PNil  :: PreCaseBranches sys dom '[] a
  PCons :: Named b IslSet
        -> Core.Expr sys b a
        -> PreCaseBranches sys dom bs a
        -> PreCaseBranches sys dom (b ': bs) a

-- | Walk a 'Surface.Branches' snoc-list, installing a fresh skolem
-- for each branch's effective domain and collecting the deferred
-- branch chain plus the list of branch domains for the partition
-- check.  The continuation closes over the existential @bs@ list of
-- branch skolems.
collectBranches
  :: forall ps pctx sys dom n (amb :: DomTag ps n) branchDoms a decls.
     ( KnownSymbols ps
     , KnownConstraints ps 0 pctx
     , KnownNat n
     , KnownDom ps n amb )
  => ElabMode
  -> Named dom IslSet
  -> VarEnv sys
  -> Surface.Branches ps pctx decls n amb branchDoms a
  -> ( forall (bs :: [Type]).
       PreCaseBranches sys dom bs a
    -> [SomeNamedSet]
    -> Either ElabError (Core.Expr sys dom a) )
  -> Either ElabError (Core.Expr sys dom a)
collectBranches _mode _ _ Surface.BNil k = k PNil []
collectBranches mode namedDom env
  (Surface.BCons (Proxy :: Proxy dBranch)
                 (body :: Surface.Expr ps pctx decls n (EffectiveDomTag dBranch amb) a)
                 rest) k =
  let brConjs = fmap (foldPctxIntoSet (pctxSetCs @ps @pctx))
                  (reflectDomConjunctions @ps @n @(EffectiveDomTag dBranch amb))
      brSet   = NamedSet
        { nsName   = Nothing
        , nsParams = symbolVals @ps
        , nsNDims  = fromIntegral (natVal (Proxy @n))
        , nsConjs  = brConjs
        }
  in name brSet $ \(namedBranch :: Named b IslSet) ->
       case walkExprAt @ps @pctx @sys @b mode namedBranch env body of
         Left err    -> Left err
         Right bodyE ->
           collectBranches @ps @pctx @sys @dom @n @amb mode namedDom env rest $
             \tailPre tailSets ->
               k (PCons namedBranch bodyE tailPre)
                 (SomeNamedSet namedBranch : tailSets)

-- | Walk a 'PreCaseBranches' chain and paste a 'Subset b dom' token
-- on each entry, derived from the overall 'Partition dom bs'.
--
-- We pass the outer partition token uniformly to every call — the
-- axiom requires /a/ 'Partition' (any branch-list) plus the branch's
-- and ambient's 'Named' payloads.  The axiom is polymorphic in its
-- partition argument's @bs@ index, so the recursive call reuses the
-- outer partition unchanged.
attachSubsets
  :: forall sys dom bs0 bs a.
     Partition dom bs0
  -> Named dom IslSet
  -> PreCaseBranches sys dom bs a
  -> Core.CaseBranches sys dom bs a
attachSubsets _    _        PNil = Core.BNil
attachSubsets part namedDom (PCons (namedB :: Named b IslSet) bodyE tl) =
  Core.BCons namedB
             (axiomBranchSubset part namedB namedDom)
             bodyE
             (attachSubsets @sys @dom @bs0 @_ @a part namedDom tl)


-- ═══════════════════════════════════════════════════════════════════════
-- §9. Restrict walker
-- ═══════════════════════════════════════════════════════════════════════
--
-- Cashes in Surface 'Restrict''s 'IslSubsetD' dict (@dOuter ⊆ dInner@)
-- to mint Core's @Subset dom src@.  The caller's @namedDom@ is the
-- outer (narrower) skolem; @namedSrc@ is freshly minted from the
-- inner (wider) DomTag.

walkRestrict
  :: forall ps pctx sys dom n dOuter dInner a decls.
     ( KnownSymbols ps
     , KnownConstraints ps 0 pctx
     , KnownNat n
     , KnownDom ps n dOuter
     , KnownDom ps n dInner
     , IslSubsetD ps n
         (LitPrepend (LiftPctxN n pctx) dOuter)
         (LitPrepend (LiftPctxN n pctx) dInner) )
  => ElabMode
  -> Named dom IslSet
  -> VarEnv sys
  -> Surface.Expr ps pctx decls n dInner a
  -> Either ElabError (Core.Expr sys dom a)
walkRestrict mode namedDom env inner =
  let nDims     = fromIntegral (natVal (Proxy @n)) :: Int
      srcConjs  = fmap (foldPctxIntoSet (pctxSetCs @ps @pctx))
                    (reflectDomConjunctions @ps @n @dInner)
      srcSet    = NamedSet
        { nsName   = Nothing
        , nsParams = symbolVals @ps
        , nsNDims  = nDims
        , nsConjs  = srcConjs
        }
  in name srcSet $ \(namedSrc :: Named src IslSet) ->
       case unsafePerformIO
              (checkAndTokenizeSubset
                 @ps @pctx @n @dOuter @dInner
                 @dom @src
                 mode
                 (Proxy @pctx) (Proxy @dOuter) (Proxy @dInner)
                 namedDom namedSrc) of
         Left err  -> Left err
         Right tok -> do
           innerE <- walkExprAt @ps @pctx @sys @src mode namedSrc env inner
           pure (Core.Restrict namedSrc tok innerE)
