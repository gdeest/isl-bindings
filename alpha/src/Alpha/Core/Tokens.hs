{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Per-relation opaque proof tokens for the Alpha Core rewrite.
--
-- Each token type is uninhabited: the data constructor is never
-- exported, so the only way to obtain a value of @Subset a b@,
-- @ImageSubset m src tgt@, etc. is through a checker that has
-- consulted ISL.  A token is therefore a certificate that the
-- named relation holds.
--
-- See the vectorized-crafting-widget plan for the surrounding design.
module Alpha.Core.Tokens
  ( -- * Token types
    Subset
  , Equal
  , Disjoint
  , ImageSubset
  , ImageEqual
  , Partition
  , DefinesAll
  , InScope
    -- * ISL exchange aliases
  , IslSet
  , IslMap
    -- * Elaboration errors
  , ElabError(..)
    -- * Checkers
  , checkSubset
  , checkEqual
  , checkDisjoint
  , checkImageSubset
  , checkImageEqual
  , checkPartition
  , checkDefinesAll
    -- * Cash-in-plugin-dict helpers and debug-path twins
    --
    -- These are the narrow trust boundary used by
    -- "Alpha.Surface.Elaborate".  Do not re-export from
    -- "Alpha.Core" — they are *only* sound in the scope where a
    -- plugin-discharged dict witnesses the corresponding ISL fact.
  , SomeNamedSet(..)
  , ElabMode(..)
  , tokenizeImageSubset
  , tokenizeImageEqual
  , tokenizePartition
  , tokenizeSubset
  , tokenizeInScope
  , axiomDomEq
  , axiomScalarEq
  , axiomBranchSubset
  , checkAndTokenizeImageSubset
  , checkAndTokenizeImageEqual
  , checkAndTokenizePartition
  , checkAndTokenizeSubset
    -- * NamedSet materialization (for downstream interpreters)
  , materializeNamedSet
  ) where

import Control.DeepSeq      (NFData(..))
import Data.Kind            (Type)
import Data.List            (nub, sort, (\\))
import Data.Proxy           (Proxy)
import Data.Type.Equality   ((:~:))
import GHC.TypeLits         (Symbol)
import System.IO.Unsafe     (unsafePerformIO)
import Unsafe.Coerce        (unsafeCoerce)

import qualified Alpha.Core.Named as Named
import Alpha.Core.Named (Named)

import Isl.Monad        (IslT, Ur(..), runIslT)
import qualified Isl.Linear as Isl
import Isl.Linear       (query_, queryM_)
import qualified Isl.Types as Isl
import Isl.Typed.Constraints
  ( NamedSet(..)
  , NamedMap(..)
  , Conjunction
  , buildBasicSet
  , buildBasicMap
  )
import qualified Isl.Typed.Constraints as C (SetIx, MapIx)
import qualified Isl.Map      as M
import qualified Isl.Set      as S
import qualified Isl.Space    as Space

import GHC.TypeLits            (Nat, type (+))
import Isl.TypeLevel.Constraint (TConstraint, LiftPctxN)
import Isl.TypeLevel.Reflection
  ( Append
  , DomTag
  , IslImageEqualD
  , IslImageSubsetD
  , IslPartitionsD
  , IslSubsetD
  , LitPrepend
  , MapLitPrepend
  )

-- ═══════════════════════════════════════════════════════════════════════
-- §1. Token types (all uninhabited — constructors unexported)
-- ═══════════════════════════════════════════════════════════════════════

-- | @Subset a b@: ISL has verified that the set named @a@ is a subset
-- of the set named @b@.
data Subset        (a :: Type) (b :: Type)

-- | @Equal a b@: the sets named @a@ and @b@ are ISL-equal.
data Equal         (a :: Type) (b :: Type)

-- | @Disjoint a b@: the sets named @a@ and @b@ have empty intersection.
data Disjoint      (a :: Type) (b :: Type)

-- | @ImageSubset m src tgt@: @image(m | src) ⊆ tgt@.
data ImageSubset   (m :: Type) (src :: Type) (tgt :: Type)

-- | @ImageEqual m src tgt@: @image(m | src) = tgt@.
data ImageEqual    (m :: Type) (src :: Type) (tgt :: Type)

-- | @Partition amb bs@: the branches @bs@ partition the ambient @amb@
-- (pairwise disjoint, covering).
data Partition     (amb :: Type) (bs :: [Type])

-- | @DefinesAll dcs eqs@: equations @eqs@ define exactly the
-- declarations @dcs@ (no missing, duplicate, or extra definitions).
data DefinesAll    (dcs :: [Type]) (eqs :: [Type])

-- | @InScope sys v@: variable @v@ is declared in system @sys@.
--
-- Constructed only at reify time by the elaborator; no standalone
-- checker is exposed.
data InScope       (sys :: Type) (v :: Symbol)

-- Tokens are phantom witnesses minted by 'mkToken' (see §4); their
-- runtime representation is a placeholder '()' coerced to the token
-- type.  'runIslT' forces its result through 'rnf', so we supply
-- trivial 'NFData' instances that don't dereference the phantom.
instance NFData (Subset a b)              where rnf _ = ()
instance NFData (Equal a b)               where rnf _ = ()
instance NFData (Disjoint a b)            where rnf _ = ()
instance NFData (ImageSubset m src tgt)   where rnf _ = ()
instance NFData (ImageEqual m src tgt)    where rnf _ = ()
instance NFData (Partition amb bs)        where rnf _ = ()
instance NFData (DefinesAll dcs eqs)      where rnf _ = ()
instance NFData (InScope sys v)           where rnf _ = ()

-- ═══════════════════════════════════════════════════════════════════════
-- §2. ISL exchange aliases
-- ═══════════════════════════════════════════════════════════════════════

-- | Value-level ISL set: reuses 'NamedSet' from "Isl.Typed.Constraints".
type IslSet = NamedSet

-- | Value-level ISL map: reuses 'NamedMap' from "Isl.Typed.Constraints".
type IslMap = NamedMap

-- ═══════════════════════════════════════════════════════════════════════
-- §3. Elaboration errors
-- ═══════════════════════════════════════════════════════════════════════

-- | Payload-bearing variants: each carries enough string context
-- (names, dimensionalities) to diagnose a checker failure downstream.
-- We deliberately avoid ISL string forms here — the shape + name is
-- enough to locate the fault in a user-facing diagnostic; rendering
-- an ISL text form (for display) is the elaborator's concern.
data ElabError
  = SubsetFails        !String !String       -- ^ @a ⊆ b@ failed; names for (a, b)
  | EqualFails         !String !String
  | DisjointFails      !String !String
  | ImageSubsetFails   !String !String !String
      -- ^ @image(m | src) ⊆ tgt@ failed; names for (m, src, tgt)
  | ImageEqualFails    !String !String !String
      -- ^ @image(m | src) = tgt@ failed; names for (m, src, tgt)
  | OutOfBoundsAccess
  | NonPartitionCover  !String               -- ^ branch-union does not cover ambient
  | NonPartitionDisjoint !Int !Int           -- ^ branches i and j overlap
  | MissingDef         !String
  | DuplicateDef       !String
  | ExtraDef           !String
  deriving (Show, Eq)

instance NFData ElabError where
  rnf e = case e of
    SubsetFails a b         -> rnf a `seq` rnf b
    EqualFails a b          -> rnf a `seq` rnf b
    DisjointFails a b       -> rnf a `seq` rnf b
    ImageSubsetFails m s t  -> rnf m `seq` rnf s `seq` rnf t
    ImageEqualFails m s t   -> rnf m `seq` rnf s `seq` rnf t
    OutOfBoundsAccess       -> ()
    NonPartitionCover s     -> rnf s
    NonPartitionDisjoint i j -> rnf i `seq` rnf j
    MissingDef s            -> rnf s
    DuplicateDef s          -> rnf s
    ExtraDef s              -> rnf s

-- ═══════════════════════════════════════════════════════════════════════
-- §4. Token fabrication
-- ═══════════════════════════════════════════════════════════════════════

-- | The sole trust boundary for this module.
--
-- Tokens are uninhabited phantom types; once a checker has verified
-- the relation against ISL, we mint a value by reinterpreting a
-- placeholder.  Because each token type has no runtime representation
-- and is phantom in all its parameters, the coercion is operationally
-- inert — it only affects the type.  Keeping the unsafeCoerce inside
-- this single helper bounds the trust surface for the whole Alpha
-- package.
mkToken :: forall a. a
mkToken = unsafeCoerce ()

-- ═══════════════════════════════════════════════════════════════════════
-- §5. NamedSet / NamedMap materialization helpers
-- ═══════════════════════════════════════════════════════════════════════
--
-- These mirror the 'domToSet' pattern in
-- @typelevel/src/Isl/TypeLevel/Reflection.hs@ but operate on
-- value-level 'NamedSet' / 'NamedMap' rather than type-level singletons.
-- They are local to this module (not exported) because all callers are
-- the checkers below.

-- | Materialize a 'NamedSet' into an ISL 'Isl.Set' (a union of its
-- disjunctions).  Parameter names are installed on the space;
-- tuple name, if any, is set so downstream ISL operations see the
-- statement identity.
materializeNamedSet :: NamedSet -> IslT IO Isl.Set
materializeNamedSet ns = Isl.do
  let paramNs = nsParams ns
      nDims   = nsNDims ns
      nParams = length paramNs
  case nsConjs ns of
    [] -> Isl.do
      space0 <- Space.setAlloc (fromIntegral nParams) (fromIntegral nDims)
      space1 <- Isl.foldM
                  (\sp (i, name) -> Space.setDimName sp Isl.islDimParam i name)
                  space0 (zip [0..] paramNs)
      space2 <- case nsName ns of
                  Just tname -> Space.setTupleName space1 Isl.islDimSet tname
                  Nothing    -> Isl.pure space1
      S.empty space2
    (c0 : cs) -> Isl.do
      s0 <- basicSetToSet paramNs nDims (nsName ns) c0
      Isl.foldM
        (\acc c' -> Isl.do
            s' <- basicSetToSet paramNs nDims (nsName ns) c'
            S.union acc s')
        s0 cs
  where
    basicSetToSet :: [String] -> Int -> Maybe String
                  -> Conjunction C.SetIx
                  -> IslT IO Isl.Set
    basicSetToSet paramNs nDims mbName conj = Isl.do
      bs0 <- buildBasicSet paramNs nDims conj
      s   <- S.fromBasicSet bs0
      case mbName of
        Just tname -> S.setTupleName s tname
        Nothing    -> Isl.pure s

-- | Materialize a 'NamedMap' into an ISL 'Isl.Map'.
materializeNamedMap :: NamedMap -> IslT IO Isl.Map
materializeNamedMap nm = Isl.do
  let paramNs = nmParams nm
      nIn     = nmNIn nm
      nOut    = nmNOut nm
      nParams = length paramNs
  case nmConjs nm of
    [] -> Isl.do
      space0 <- Space.alloc (fromIntegral nParams) (fromIntegral nIn) (fromIntegral nOut)
      space1 <- Isl.foldM
                  (\sp (i, name) -> Space.setDimName sp Isl.islDimParam i name)
                  space0 (zip [0..] paramNs)
      space2 <- case nmDomainName nm of
                  Just dn -> Space.setTupleName space1 Isl.islDimIn dn
                  Nothing -> Isl.pure space1
      space3 <- case nmRangeName nm of
                  Just rn -> Space.setTupleName space2 Isl.islDimOut rn
                  Nothing -> Isl.pure space2
      M.empty space3
    (c0 : cs) -> Isl.do
      m0 <- basicMapToMap paramNs nIn nOut (nmDomainName nm) (nmRangeName nm) c0
      Isl.foldM
        (\acc c' -> Isl.do
            m' <- basicMapToMap paramNs nIn nOut (nmDomainName nm) (nmRangeName nm) c'
            M.union acc m')
        m0 cs
  where
    basicMapToMap :: [String] -> Int -> Int -> Maybe String -> Maybe String
                  -> Conjunction C.MapIx
                  -> IslT IO Isl.Map
    basicMapToMap paramNs nIn nOut domN rngN conj = Isl.do
      bm0 <- buildBasicMap paramNs nIn nOut conj
      m   <- M.fromBasicMap bm0
      m1  <- case domN of
               Just dn -> M.setTupleName m Isl.islDimIn dn
               Nothing -> Isl.pure m
      case rngN of
        Just rn -> M.setTupleName m1 Isl.islDimOut rn
        Nothing -> Isl.pure m1

-- ═══════════════════════════════════════════════════════════════════════
-- §6. Borrow pair helpers — run a binary ISL predicate across two owned
-- sets, freeing both.
-- ═══════════════════════════════════════════════════════════════════════

-- | Borrow two owned sets, apply a pure binary predicate over their
-- refs, and free both.  Used by every two-set checker below; factored
-- to keep the linearity discipline in one place.
withTwoSets
  :: Isl.Set %1 -> Isl.Set %1
  -> (forall sA sB. Isl.SetRef sA -> Isl.SetRef sB -> Bool)
  -> IslT IO (Ur Bool)
withTwoSets a b pred_ =
  queryM_ a (\aR -> query_ b (\bR -> pred_ aR bR))

-- ═══════════════════════════════════════════════════════════════════════
-- §7. Helpers: diagnostic labels
-- ═══════════════════════════════════════════════════════════════════════

setLabel :: NamedSet -> String
setLabel = maybe "<anon>" id . nsName

mapLabel :: NamedMap -> String
mapLabel nm = maybe "<anon>" id (nmDomainName nm)
           ++ " -> "
           ++ maybe "<anon>" id (nmRangeName nm)

-- ═══════════════════════════════════════════════════════════════════════
-- §8. Checkers
-- ═══════════════════════════════════════════════════════════════════════

-- | @a ⊆ b@.
checkSubset
  :: forall a b
   . IslSet -> IslSet -> IO (Either ElabError (Subset a b))
checkSubset aNS bNS = pure $ unsafePerformIO $ runIslT $ Isl.do
  aObj  <- materializeNamedSet aNS
  bObj  <- materializeNamedSet bNS
  Ur ok <- withTwoSets aObj bObj S.isSubset
  if ok
    then Isl.pure (Ur (Right (mkToken :: Subset a b)))
    else Isl.pure (Ur (Left (SubsetFails (setLabel aNS) (setLabel bNS))))
{-# NOINLINE checkSubset #-}

-- | @a = b@.
checkEqual
  :: forall a b
   . IslSet -> IslSet -> IO (Either ElabError (Equal a b))
checkEqual aNS bNS = pure $ unsafePerformIO $ runIslT $ Isl.do
  aObj  <- materializeNamedSet aNS
  bObj  <- materializeNamedSet bNS
  Ur ok <- withTwoSets aObj bObj S.isEqual
  if ok
    then Isl.pure (Ur (Right (mkToken :: Equal a b)))
    else Isl.pure (Ur (Left (EqualFails (setLabel aNS) (setLabel bNS))))
{-# NOINLINE checkEqual #-}

-- | @a ∩ b = ∅@.
checkDisjoint
  :: forall a b
   . IslSet -> IslSet -> IO (Either ElabError (Disjoint a b))
checkDisjoint aNS bNS = pure $ unsafePerformIO $ runIslT $ Isl.do
  aObj  <- materializeNamedSet aNS
  bObj  <- materializeNamedSet bNS
  Ur ok <- withTwoSets aObj bObj S.isDisjoint
  if ok
    then Isl.pure (Ur (Right (mkToken :: Disjoint a b)))
    else Isl.pure (Ur (Left (DisjointFails (setLabel aNS) (setLabel bNS))))
{-# NOINLINE checkDisjoint #-}

-- | @image(m | src) ⊆ tgt@ — computed as @apply(src, m) ⊆ tgt@.
checkImageSubset
  :: forall m src tgt
   . IslMap -> IslSet -> IslSet
  -> IO (Either ElabError (ImageSubset m src tgt))
checkImageSubset mm srcNS tgtNS = pure $ unsafePerformIO $ runIslT $ Isl.do
  mapObj <- materializeNamedMap mm
  srcObj <- materializeNamedSet srcNS
  tgtObj <- materializeNamedSet tgtNS
  img    <- S.apply srcObj mapObj
  Ur ok  <- withTwoSets img tgtObj S.isSubset
  if ok
    then Isl.pure (Ur (Right (mkToken :: ImageSubset m src tgt)))
    else Isl.pure (Ur (Left (ImageSubsetFails (mapLabel mm)
                                              (setLabel srcNS)
                                              (setLabel tgtNS))))
{-# NOINLINE checkImageSubset #-}

-- | @image(m | src) = tgt@ — computed as @apply(src, m) = tgt@.
checkImageEqual
  :: forall m src tgt
   . IslMap -> IslSet -> IslSet
  -> IO (Either ElabError (ImageEqual m src tgt))
checkImageEqual mm srcNS tgtNS = pure $ unsafePerformIO $ runIslT $ Isl.do
  mapObj <- materializeNamedMap mm
  srcObj <- materializeNamedSet srcNS
  tgtObj <- materializeNamedSet tgtNS
  img    <- S.apply srcObj mapObj
  Ur ok  <- withTwoSets img tgtObj S.isEqual
  if ok
    then Isl.pure (Ur (Right (mkToken :: ImageEqual m src tgt)))
    else Isl.pure (Ur (Left (ImageEqualFails (mapLabel mm)
                                             (setLabel srcNS)
                                             (setLabel tgtNS))))
{-# NOINLINE checkImageEqual #-}

-- | @{bs}@ partition @amb@: pairwise disjoint, and @amb = ⋃ bs@.
checkPartition
  :: forall amb bs
   . IslSet -> [IslSet]
  -> IO (Either ElabError (Partition amb bs))
checkPartition ambNS bsNS = pure $ unsafePerformIO $ runIslT $ partitionIO ambNS bsNS
{-# NOINLINE checkPartition #-}

-- | ISL body of 'checkPartition'.  Factored out so the outer
-- continuation can be a single @Isl.do@ block without guard-vs-case
-- parsing ambiguities.
partitionIO
  :: forall amb bs
   . IslSet -> [IslSet]
  -> IslT IO (Ur (Either ElabError (Partition amb bs)))
partitionIO ambNS bsNS = Isl.do
  Ur pairRes <- firstOverlap 0 bsNS
  case pairRes of
    Just (i, j) ->
      Isl.pure (Ur (Left (NonPartitionDisjoint i j)))
    Nothing ->
      if null bsNS
        then emptyBranchCover ambNS
        else Isl.do
          Ur covered <- coverageCheck ambNS bsNS
          if covered
            then Isl.pure (Ur (Right (mkToken :: Partition amb bs)))
            else Isl.pure (Ur (Left (NonPartitionCover (setLabel ambNS))))

-- | Zero-branch edge case: covers iff the ambient is empty.
emptyBranchCover
  :: forall amb bs
   . IslSet -> IslT IO (Ur (Either ElabError (Partition amb bs)))
emptyBranchCover ambNS = Isl.do
  ambObj <- materializeNamedSet ambNS
  Ur isE <- query_ ambObj S.isEmpty
  if isE
    then Isl.pure (Ur (Right (mkToken :: Partition amb bs)))
    else Isl.pure (Ur (Left (NonPartitionCover (setLabel ambNS))))

-- | Scan branch list for the first overlap @(i, j)@ with @i < j@.
firstOverlap :: Int -> [NamedSet] -> IslT IO (Ur (Maybe (Int, Int)))
firstOverlap _ []       = Isl.pure (Ur Nothing)
firstOverlap _ [_]      = Isl.pure (Ur Nothing)
firstOverlap i (x : xs) = Isl.do
  Ur res <- checkAgainst i (i + 1) x xs
  case res of
    Just _  -> Isl.pure (Ur res)
    Nothing -> firstOverlap (i + 1) xs

-- | For a fixed left index @i@, scan the tail list for an overlap.
checkAgainst
  :: Int -> Int -> NamedSet -> [NamedSet]
  -> IslT IO (Ur (Maybe (Int, Int)))
checkAgainst _ _  _ []       = Isl.pure (Ur Nothing)
checkAgainst i j0 x (y : ys) = Isl.do
  xObj <- materializeNamedSet x
  yObj <- materializeNamedSet y
  Ur disj <- withTwoSets xObj yObj S.isDisjoint
  if not disj
    then Isl.pure (Ur (Just (i, j0)))
    else checkAgainst i (j0 + 1) x ys

-- | Mutual-subset coverage check.  Rematerializes each object per
-- ISL call: the predicates consume owned sets, so we need fresh
-- objects for the second direction.  Inefficient in the common case
-- (two ISL builds of the same union) but correct; a future
-- elaborator-level intern cache would batch these.
coverageCheck :: NamedSet -> [NamedSet] -> IslT IO (Ur Bool)
coverageCheck _     [] = Isl.pure (Ur True)
coverageCheck ambNS bs = Isl.do
  ambObj     <- materializeNamedSet ambNS
  branchObj  <- buildBranchUnion bs
  Ur ok1     <- withTwoSets ambObj branchObj S.isSubset
  ambObj'    <- materializeNamedSet ambNS
  branchObj' <- buildBranchUnion bs
  Ur ok2     <- withTwoSets branchObj' ambObj' S.isSubset
  Isl.pure (Ur (ok1 && ok2))

buildBranchUnion :: [NamedSet] -> IslT IO Isl.Set
buildBranchUnion [] = error "buildBranchUnion: empty list (caller must guard)"
buildBranchUnion (b0 : rest) = Isl.do
  s0 <- materializeNamedSet b0
  Isl.foldM
    (\acc nb -> Isl.do
        s' <- materializeNamedSet nb
        S.union acc s')
    s0 rest

-- | Pure string-set totality check: every declared name must have
-- exactly one defining equation.
checkDefinesAll
  :: forall dcs eqs
   . [String]            -- ^ declared (outputs ++ locals)
  -> [String]            -- ^ equation names
  -> Either ElabError (DefinesAll dcs eqs)
checkDefinesAll dcs eqs =
  case firstDup (sort eqs) of
    Just d  -> Left (DuplicateDef d)
    Nothing ->
      case (sort (nub dcs) \\ sort (nub eqs), sort (nub eqs) \\ sort (nub dcs)) of
        ([], [])   -> Right (mkToken :: DefinesAll dcs eqs)
        (m:_, _)   -> Left (MissingDef m)
        (_,  x:_)  -> Left (ExtraDef   x)
  where
    firstDup (x:y:rest) | x == y    = Just x
                        | otherwise = firstDup (y:rest)
    firstDup _                      = Nothing

-- ═══════════════════════════════════════════════════════════════════════
-- §9. Plugin-dict cash-in helpers (NOT re-exported via Alpha.Core)
-- ═══════════════════════════════════════════════════════════════════════
--
-- Each helper takes the plugin-discharged obligation as a /constraint/
-- and returns the corresponding uninhabited token via 'mkToken'.  The
-- constraint is structural evidence that the caller's scope has the
-- fact already proved; no new unsafeCoerce is introduced here — all
-- token fabrication goes through the existing 'mkToken' seal.
--
-- TRUST BOUNDARY.  The caller (currently only
-- "Alpha.Surface.Elaborate") must have /materialised/ its 'Named'
-- payloads so that they correspond to the same ISL content the
-- plugin's dict is witnessing about.  The 'SanityCheck' variants
-- below verify this correspondence at runtime for CI.

-- | A 'Named' 'IslSet' with the skolem hidden.  Used by the partition
-- helpers to accept a dynamic list of branch domains.
data SomeNamedSet where
  SomeNamedSet :: Named b IslSet -> SomeNamedSet

-- | Elaboration mode.  'TrustPlugin' mints tokens via 'mkToken'
-- straight from the plugin dict; 'SanityCheck' additionally runs the
-- ISL-backed checker and fails if the plugin's claim disagrees.
data ElabMode = TrustPlugin | SanityCheck
  deriving (Eq, Show)


-- | Cash in an @IslImageSubsetD@ plugin dict: mint the corresponding
-- 'ImageSubset m src tgt' token.  The 'Named' arguments carry the
-- materialised map/src/tgt payloads that the SanityCheck variant can
-- re-verify; in TrustPlugin mode they only serve as structural shape
-- evidence (the token itself is uninhabited).
tokenizeImageSubset
  :: forall (ps :: [Symbol]) (pctx :: [TConstraint ps 0])
            (ni :: Nat) (no :: Nat)
            (mapCs :: [TConstraint ps (ni + no)])
            (dSrc :: DomTag ps ni) (dDst :: DomTag ps no)
            m src tgt.
     IslImageSubsetD ps ni no
       (Append (LiftPctxN (ni + no) pctx) mapCs)
       (LitPrepend (LiftPctxN ni pctx) dSrc)
       (LitPrepend (LiftPctxN no pctx) dDst)
  => Proxy pctx
  -> Proxy mapCs
  -> Proxy dSrc
  -> Proxy dDst
  -> Named m   IslMap
  -> Named src IslSet
  -> Named tgt IslSet
  -> ImageSubset m src tgt
tokenizeImageSubset _ _ _ _ _ _ _ = mkToken
  -- TRUST: the IslImageSubsetD dict witnesses image(mapCs | dSrc) ⊆ dDst
  -- at the fused-pctx obligation shape.  The elaborator passes Named
  -- payloads built by materialising the same type-level structure.
{-# INLINE tokenizeImageSubset #-}

-- | Cash in an @IslImageEqualD@ plugin dict: mint the corresponding
-- 'ImageEqual m src tgt' token.  Mirrors 'tokenizeImageSubset' line for
-- line; the only difference is the equality polarity of the underlying
-- obligation (the plugin proves @image(m | src) = tgt@ rather than
-- @image(m | src) ⊆ tgt@).
tokenizeImageEqual
  :: forall (ps :: [Symbol]) (pctx :: [TConstraint ps 0])
            (ni :: Nat) (no :: Nat)
            (mapCs :: [TConstraint ps (ni + no)])
            (dSrc :: DomTag ps ni) (dDst :: DomTag ps no)
            m src tgt.
     IslImageEqualD ps ni no
       (Append (LiftPctxN (ni + no) pctx) mapCs)
       (LitPrepend (LiftPctxN ni pctx) dSrc)
       (LitPrepend (LiftPctxN no pctx) dDst)
  => Proxy pctx
  -> Proxy mapCs
  -> Proxy dSrc
  -> Proxy dDst
  -> Named m   IslMap
  -> Named src IslSet
  -> Named tgt IslSet
  -> ImageEqual m src tgt
tokenizeImageEqual _ _ _ _ _ _ _ = mkToken
  -- TRUST: the IslImageEqualD dict witnesses image(mapCs | dSrc) = dDst
  -- at the fused-pctx obligation shape.  The elaborator passes Named
  -- payloads built by materialising the same type-level structure.
{-# INLINE tokenizeImageEqual #-}

-- | Cash in an @IslPartitionsD@ plugin dict: mint the corresponding
-- 'Partition dom bs' token.  The list of 'SomeNamedSet' carries the
-- materialised branch domains (consumed by the SanityCheck twin).
tokenizePartition
  :: forall (ps :: [Symbol]) (pctx :: [TConstraint ps 0])
            (n :: Nat)
            (d :: DomTag ps n) (branches :: [DomTag ps n])
            dom bs.
     IslPartitionsD ps n
       (LitPrepend (LiftPctxN n pctx) d)
       (MapLitPrepend (LiftPctxN n pctx) branches)
  => Proxy pctx
  -> Proxy d
  -> Proxy branches
  -> Named dom IslSet
  -> [SomeNamedSet]
  -> Partition dom bs
tokenizePartition _ _ _ _ _ = mkToken
  -- TRUST: the IslPartitionsD dict witnesses that @branches@ partition
  -- @d@ at the fused-pctx obligation shape.  The @bs@ type-level index
  -- on the minted token is determined by the elaborator's recursion
  -- over the surface branch list.
{-# INLINE tokenizePartition #-}

-- | Cash in an @IslSubsetD@ plugin dict: mint the corresponding
-- 'Subset a b' token.
tokenizeSubset
  :: forall (ps :: [Symbol]) (pctx :: [TConstraint ps 0])
            (n :: Nat)
            (d1 :: DomTag ps n) (d2 :: DomTag ps n) a b.
     IslSubsetD ps n
       (LitPrepend (LiftPctxN n pctx) d1)
       (LitPrepend (LiftPctxN n pctx) d2)
  => Proxy pctx
  -> Proxy d1
  -> Proxy d2
  -> Named a IslSet
  -> Named b IslSet
  -> Subset a b
tokenizeSubset _ _ _ _ _ = mkToken
  -- TRUST: the IslSubsetD dict witnesses d1 ⊆ d2 at the fused-pctx
  -- shape.  Used by the Case elaborator to certify each branch's
  -- effective domain is contained in the ambient.
{-# INLINE tokenizeSubset #-}

-- | Mint an 'InScope sys v' token.  Sound only when invoked by the
-- elaborator immediately after installing the variable into the
-- system skolem @sys@ — "v is declared in sys" is witnessed by the
-- calling context, not by a plugin dict.
tokenizeInScope :: forall sys v. Proxy v -> InScope sys v
tokenizeInScope _ = mkToken
  -- TRUST: caller (Alpha.Surface.Elaborate.installDecls) invokes this
  -- exactly once per declared variable, inside the scope where the
  -- VarDecl sys v _ has just been introduced.
{-# INLINE tokenizeInScope #-}

-- | Coerce between two 'Named' skolems labelling the /same/ IslSet
-- content.  The equality witness is minted through 'mkToken' (unit
-- coercion) — operationally inert because skolem identity is the
-- only thing being unified and both skolems refer to the same payload.
--
-- Used by the elaborator's 'Var' case: a surface 'Var' references a
-- declared variable whose skolem is the @VarDecl@'s @dom@, but the
-- enclosing Dep/Reduce/Case has installed a fresh @src@ skolem for
-- the body's current ambient — the two skolems label the same ISL
-- content (the plugin has structurally unified their DomTag phantoms),
-- so this axiom bridges them without allocating a new 'Named' layer.
--
-- TRUST: only the elaborator invokes this, and only when the two
-- 'Named' payloads are structurally equal.
axiomDomEq :: forall d1 d2. Named d1 IslSet -> Named d2 IslSet -> d1 :~: d2
axiomDomEq _ _ = mkToken
{-# INLINE axiomDomEq #-}

-- | Bridge a scalar-type mismatch between a surface equation's body
-- type (@DeclType decl@) and the monomorphic @a@ the elaborator's
-- caller chose for its result 'System sys a'.
--
-- The Surface 'Defines' constructor forces the body's scalar to match
-- the declared variable's 'DeclType', but the Core 'System sys a' has
-- one uniform @a@ across all equations.  The elaborator asserts that
-- the caller's @a@ matches every equation's 'DeclType' (a contract of
-- the 'elaborate' entry point) and closes the gap via this axiom.
--
-- TRUST: caller ('Alpha.Surface.Elaborate.elaborate') takes @a@ as an
-- explicit type argument; any equation whose body's 'DeclType' is not
-- @a@ violates the contract.  A future revision could replace this
-- with per-equation scalar existentials in 'SomeEquation', eliminating
-- the axiom.
axiomScalarEq :: forall a b. a :~: b
axiomScalarEq = mkToken
{-# INLINE axiomScalarEq #-}

-- | Given a 'Partition dom bs' witness and a chosen branch skolem
-- @b@ known to be one of the @bs@ (by the caller's recursion over the
-- Surface branch list at the same 'BCons' site that produced the
-- 'Partition' token), mint the derived @Subset b dom@ fact.
--
-- This is a logical consequence of 'Partition' — a partition's
-- branches are /individually/ subsets of the ambient — not a new ISL
-- fact.  No additional ISL call is needed; 'mkToken' reuses the sole
-- trust boundary in §4.
--
-- TRUST: caller (only "Alpha.Surface.Elaborate") applies this to a
-- branch whose 'Named b IslSet' was minted at the /same/ 'BCons' walk
-- that produced the 'Partition' token, so the @b@ is provably one of
-- the @bs@ the partition covers.
axiomBranchSubset
  :: forall dom bs b.
     Partition dom bs
  -> Named b   IslSet
  -> Named dom IslSet
  -> Subset b dom
axiomBranchSubset _ _ _ = mkToken
{-# INLINE axiomBranchSubset #-}

-- ═══════════════════════════════════════════════════════════════════════
-- §10. Debug-path twins — run the ISL checker and mint on success
-- ═══════════════════════════════════════════════════════════════════════

-- | @SanityCheck@ variant of 'tokenizeImageSubset': calls the
-- ISL-backed 'checkImageSubset' and returns its verdict.  In
-- 'TrustPlugin' mode, this is identical to the type-level tokenize
-- path (wraps 'mkToken' in @Right@).
--
-- The IslT wrapper is vestigial (the underlying checker uses its own
-- internal 'runIslT'); the type is structured this way so that the
-- elaborator can thread its 'IslT IO' context uniformly.
checkAndTokenizeImageSubset
  :: forall (ps :: [Symbol]) (pctx :: [TConstraint ps 0])
            (ni :: Nat) (no :: Nat)
            (mapCs :: [TConstraint ps (ni + no)])
            (dSrc :: DomTag ps ni) (dDst :: DomTag ps no)
            m src tgt.
     IslImageSubsetD ps ni no
       (Append (LiftPctxN (ni + no) pctx) mapCs)
       (LitPrepend (LiftPctxN ni pctx) dSrc)
       (LitPrepend (LiftPctxN no pctx) dDst)
  => ElabMode
  -> Proxy pctx
  -> Proxy mapCs
  -> Proxy dSrc
  -> Proxy dDst
  -> Named m   IslMap
  -> Named src IslSet
  -> Named tgt IslSet
  -> IO (Either ElabError (ImageSubset m src tgt))
checkAndTokenizeImageSubset TrustPlugin ppp pmc pSrc pDst nM nSrc nTgt =
  pure (Right (tokenizeImageSubset @ps @pctx @ni @no @mapCs @dSrc @dDst
                 ppp pmc pSrc pDst nM nSrc nTgt))
checkAndTokenizeImageSubset SanityCheck _ _ _ _ nM nSrc nTgt =
  -- Strip tuple names before the ISL check: ISL's isl_set_apply
  -- compares spaces including tuple names, but skolem identity is
  -- carried entirely at the type level — payloads that match
  -- structurally but differ in tuple names must still pass.
  checkImageSubset (stripMapNames (the' nM))
                   (stripSetName (the' nSrc))
                   (stripSetName (the' nTgt))

-- | @SanityCheck@ variant of 'tokenizeImageEqual': calls the
-- ISL-backed 'checkImageEqual' and returns its verdict.  In
-- 'TrustPlugin' mode, this is identical to the type-level tokenize
-- path (wraps 'mkToken' in @Right@).
checkAndTokenizeImageEqual
  :: forall (ps :: [Symbol]) (pctx :: [TConstraint ps 0])
            (ni :: Nat) (no :: Nat)
            (mapCs :: [TConstraint ps (ni + no)])
            (dSrc :: DomTag ps ni) (dDst :: DomTag ps no)
            m src tgt.
     IslImageEqualD ps ni no
       (Append (LiftPctxN (ni + no) pctx) mapCs)
       (LitPrepend (LiftPctxN ni pctx) dSrc)
       (LitPrepend (LiftPctxN no pctx) dDst)
  => ElabMode
  -> Proxy pctx
  -> Proxy mapCs
  -> Proxy dSrc
  -> Proxy dDst
  -> Named m   IslMap
  -> Named src IslSet
  -> Named tgt IslSet
  -> IO (Either ElabError (ImageEqual m src tgt))
checkAndTokenizeImageEqual TrustPlugin ppp pmc pSrc pDst nM nSrc nTgt =
  pure (Right (tokenizeImageEqual @ps @pctx @ni @no @mapCs @dSrc @dDst
                 ppp pmc pSrc pDst nM nSrc nTgt))
checkAndTokenizeImageEqual SanityCheck _ _ _ _ nM nSrc nTgt =
  -- Strip tuple names before the ISL check: ISL's isl_set_apply
  -- compares spaces including tuple names, but skolem identity is
  -- carried entirely at the type level — payloads that match
  -- structurally but differ in tuple names must still pass.
  checkImageEqual (stripMapNames (the' nM))
                  (stripSetName (the' nSrc))
                  (stripSetName (the' nTgt))

-- | Sanity-check twin of 'tokenizePartition'.
checkAndTokenizePartition
  :: forall (ps :: [Symbol]) (pctx :: [TConstraint ps 0])
            (n :: Nat)
            (d :: DomTag ps n) (branches :: [DomTag ps n])
            dom bs.
     IslPartitionsD ps n
       (LitPrepend (LiftPctxN n pctx) d)
       (MapLitPrepend (LiftPctxN n pctx) branches)
  => ElabMode
  -> Proxy pctx
  -> Proxy d
  -> Proxy branches
  -> Named dom IslSet
  -> [SomeNamedSet]
  -> IO (Either ElabError (Partition dom bs))
checkAndTokenizePartition TrustPlugin pp pd pb nDom bs =
  pure (Right (tokenizePartition @ps @pctx @n @d @branches @dom @bs
                 pp pd pb nDom bs))
checkAndTokenizePartition SanityCheck _ _ _ nDom bs =
  checkPartition (stripSetName (the' nDom))
                 (map (\(SomeNamedSet b) -> stripSetName (the' b)) bs)

-- | Sanity-check twin of 'tokenizeSubset'.
checkAndTokenizeSubset
  :: forall (ps :: [Symbol]) (pctx :: [TConstraint ps 0])
            (n :: Nat)
            (d1 :: DomTag ps n) (d2 :: DomTag ps n) a b.
     IslSubsetD ps n
       (LitPrepend (LiftPctxN n pctx) d1)
       (LitPrepend (LiftPctxN n pctx) d2)
  => ElabMode
  -> Proxy pctx
  -> Proxy d1
  -> Proxy d2
  -> Named a IslSet
  -> Named b IslSet
  -> IO (Either ElabError (Subset a b))
checkAndTokenizeSubset TrustPlugin pp pd1 pd2 nA nB =
  pure (Right (tokenizeSubset @ps @pctx @n @d1 @d2 @a @b pp pd1 pd2 nA nB))
checkAndTokenizeSubset SanityCheck _ _ _ nA nB =
  checkSubset (stripSetName (the' nA)) (stripSetName (the' nB))

-- | Local @the@ so the implementation doesn't need to re-export
-- "Alpha.Core.Named".  The checkers take raw 'NamedSet'/'NamedMap';
-- unwrap the skolem layer via the public projector.
the' :: Named n a -> a
the' = Named.the

-- | Strip the set's tuple name.  Sanity-check path only: ISL compares
-- spaces including tuple names, but skolem identity is carried
-- entirely at the type level; payloads with different declarative
-- names can still refer to the same ISL set.
stripSetName :: NamedSet -> NamedSet
stripSetName ns = ns { nsName = Nothing }

-- | Strip the map's domain/range tuple names.  Same motivation as
-- 'stripSetName'.
stripMapNames :: NamedMap -> NamedMap
stripMapNames nm = nm { nmDomainName = Nothing, nmRangeName = Nothing }
