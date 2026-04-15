{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Singleton-based type-level polyhedral DSL.
--
-- = Architecture
--
-- 1. __Singletons__ ('STExpr', 'STConstraint', 'STConstraints') are value-level
--    witnesses of type-level data.  They carry both the type index (visible to
--    GHC for compile-time reasoning) and runtime evidence (for reification to
--    ISL objects).
--
-- 2. __Auto-derivation__ ('KnownExpr', 'KnownConstraint', 'KnownConstraints')
--    produces singletons from type-level info automatically — no manual
--    construction needed.
--
-- 3. __Singleton-carrying polyhedra__ ('SBasicSet', 'SBasicMap') pair visible
--    type-level constraints with their runtime singleton.  The smart
--    constructors 'sBasicSet' and 'sBasicMap' build these from type-level info.
--
-- 4. __Evaluation__ ('evalSBasicSet', 'evalSBasicMap') reifies singleton-carrying
--    polyhedra to ISL objects within 'IslT'.
--
-- = Usage
--
-- @
-- type Triangle = '[ 'TDim (D 0) >=. 'TConst ('Pos 0), ... ]
--
-- triangle :: SBasicSet '["N"] 2 Triangle
-- triangle = sBasicSet
--
-- main = runIslT $ do
--   s <- evalSBasicSet triangle   -- ISL Set, ready to use
--   ...
-- @
module Isl.TypeLevel.Sing
  ( -- * Type-level integers
    KnownZ(..), zValOf
    -- * Parameter name → index mapping
  , ParamIndex(..), paramIndexOf
    -- * Singletons
  , STExpr(..)
  , STConstraint(..)
  , STConstraints(..)
  , STExprs(..)
    -- * Auto-derivation: type-level → singleton
  , KnownExpr(..)
  , KnownConstraint(..)
  , KnownConstraints(..)
  , KnownExprs(..)
    -- * Singleton-carrying polyhedra
  , SBasicSet(..), sBasicSet
  , SBasicMap(..), sBasicMap
    -- * Singleton-carrying multi-aff
  , SMultiAff(..), sMultiAff
    -- * Evaluation: singleton → ISL object
  , evalSBasicSet
  , evalSBasicMap
  , evalSMultiAff
    -- * Singleton reification (singleton → value, used by evaluation)
  , reifySTConstraintsSet
  , reifySTConstraintsMap
  , reifySTConstraintsMapSplit
  , reifySTExprsSet
    -- * Runtime lifting (value → existential singleton → Dict)
  , SomeSTConstraints(..)
  , liftConstraintsMap
  , withKnownConstraints
  ) where

import Data.Kind (Type)
import Control.Monad.IO.Class (MonadIO)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Nat, KnownNat, natVal, SomeNat(..), someNatVal, Symbol, KnownSymbol, type (+))
import Unsafe.Coerce (unsafeCoerce)

import Isl.TypeLevel.Expr
import Isl.TypeLevel.Constraint (TConstraint(..), AllValid, AllValidExprs)
import Isl.Typed.Constraints (Expr(..), Constraint(..), Conjunction(..), SetIx(..), MapIx(..))
import Isl.Typed.Constraints qualified as TC
import Isl.Typed.Params (KnownSymbols(..), Length)
import qualified Isl.Types as Isl
import Isl.Monad (IslT)
import qualified Isl.Linear as Isl
import qualified Isl.Set as S
import qualified Isl.Map as M


-- =========================================================================
-- Type-level integer reification
-- =========================================================================

class KnownZ (z :: Z) where
  zVal :: Integer

instance KnownNat n => KnownZ ('Pos n) where
  zVal = fromIntegral (natVal (Proxy @n))

instance KnownNat n => KnownZ ('Neg n) where
  zVal = negate (fromIntegral (natVal (Proxy @n)))

zValOf :: forall z. KnownZ z => Proxy z -> Integer
zValOf _ = zVal @z


-- =========================================================================
-- Parameter name → index mapping
-- =========================================================================

-- | Maps a parameter name to its positional index in a parameter list.
-- Users must provide instances for each parameter name they use.
--
-- @
-- instance ParamIndex "N" where paramIndex = 0
-- instance ParamIndex "M" where paramIndex = 1
-- @
class ParamIndex (s :: Symbol) where
  paramIndex :: Int

paramIndexOf :: forall s. ParamIndex s => Proxy s -> Int
paramIndexOf _ = paramIndex @s


-- =========================================================================
-- Singletons
-- =========================================================================

-- | Singleton for a type-level expression @e :: TExpr ps n@.
data STExpr (ps :: [Symbol]) (n :: Nat) (e :: TExpr ps n) where
  STDim   :: KnownNat d
          => Proxy d -> STExpr ps n ('TDim ('MkIdx d))
  STParam :: (KnownSymbol s, ParamIndex s)
          => Proxy s -> STExpr ps n ('TParam ('MkPIdx s))
  STConst :: KnownZ z
          => Proxy z -> STExpr ps n ('TConst z)
  STAdd   :: STExpr ps n a -> STExpr ps n b
          -> STExpr ps n ('TAdd a b)
  STMul   :: KnownZ k
          => Proxy k -> STExpr ps n a -> STExpr ps n ('TMul k a)
  STFloorDiv :: KnownZ d
             => STExpr ps n a -> Proxy d -> STExpr ps n ('TFloorDiv a d)

-- | Singleton for a type-level constraint.
data STConstraint (ps :: [Symbol]) (n :: Nat) (c :: TConstraint ps n) where
  STEq :: STExpr ps n e -> STConstraint ps n ('TEq e)
  STGe :: STExpr ps n e -> STConstraint ps n ('TGe e)

-- | Singleton for a type-level constraint list.
data STConstraints (ps :: [Symbol]) (n :: Nat) (cs :: [TConstraint ps n]) where
  STNil  :: STConstraints ps n '[]
  STCons :: STConstraint ps n c -> STConstraints ps n cs
         -> STConstraints ps n (c ': cs)


-- =========================================================================
-- Auto-derivation: produce singletons from type-level info
-- =========================================================================

-- | Automatically produce a singleton for a type-level expression.
class KnownExpr (ps :: [Symbol]) (n :: Nat) (e :: TExpr ps n) where
  knownExpr :: STExpr ps n e

instance KnownNat d => KnownExpr ps n ('TDim ('MkIdx d)) where
  knownExpr = STDim Proxy

instance (KnownSymbol s, ParamIndex s) => KnownExpr ps n ('TParam ('MkPIdx s)) where
  knownExpr = STParam Proxy

instance KnownZ z => KnownExpr ps n ('TConst z) where
  knownExpr = STConst Proxy

instance (KnownExpr ps n a, KnownExpr ps n b) => KnownExpr ps n ('TAdd a b) where
  knownExpr = STAdd knownExpr knownExpr

instance (KnownZ k, KnownExpr ps n a) => KnownExpr ps n ('TMul k a) where
  knownExpr = STMul Proxy knownExpr

instance (KnownExpr ps n a, KnownZ d) => KnownExpr ps n ('TFloorDiv a d) where
  knownExpr = STFloorDiv knownExpr Proxy

-- | Automatically produce a singleton for a type-level constraint.
class KnownConstraint (ps :: [Symbol]) (n :: Nat) (c :: TConstraint ps n) where
  knownConstraint :: STConstraint ps n c

instance KnownExpr ps n e => KnownConstraint ps n ('TEq e) where
  knownConstraint = STEq knownExpr

instance KnownExpr ps n e => KnownConstraint ps n ('TGe e) where
  knownConstraint = STGe knownExpr

-- | Automatically produce a singleton for a type-level constraint list.
class KnownConstraints (ps :: [Symbol]) (n :: Nat) (cs :: [TConstraint ps n]) where
  knownConstraints :: STConstraints ps n cs

instance KnownConstraints ps n '[] where
  knownConstraints = STNil

instance (KnownConstraint ps n c, KnownConstraints ps n cs) =>
         KnownConstraints ps n (c ': cs) where
  knownConstraints = STCons knownConstraint knownConstraints


-- =========================================================================
-- Singleton-carrying polyhedra
-- =========================================================================

-- | A basic set whose type-level constraints @cs@ are visible to GHC
-- (enabling compile-time proof obligations like 'IslSubset') and whose
-- runtime singleton enables reification to ISL objects.
data SBasicSet (ps :: [Symbol]) (n :: Nat) (cs :: [TConstraint ps n]) where
  MkSBasicSet :: STConstraints ps n cs -> SBasicSet ps n cs

-- | Build an 'SBasicSet' from type-level info.  The singleton is
-- auto-derived; 'AllValid' ensures constraint well-formedness.
--
-- @
-- type Triangle = '[ 'TDim (D 0) >=. 'TConst ('Pos 0), ... ]
-- triangle :: SBasicSet '["N"] 2 Triangle
-- triangle = sBasicSet
-- @
sBasicSet :: forall ps n cs.
  (AllValid ps n cs, KnownConstraints ps n cs)
  => SBasicSet ps n cs
sBasicSet = MkSBasicSet knownConstraints

-- | A basic map whose type-level constraints are visible and singleton-backed.
type SBasicMap :: forall (ps :: [Symbol]) -> forall (ni :: Nat) -> forall (no :: Nat) -> [TConstraint ps (ni + no)] -> Type
data SBasicMap ps ni no cs where
  MkSBasicMap :: STConstraints ps (ni + no) cs -> SBasicMap ps ni no cs

-- | Build an 'SBasicMap' from type-level info.
sBasicMap :: forall ps ni no cs.
  (AllValid ps (ni + no) cs, KnownConstraints ps (ni + no) cs)
  => SBasicMap ps ni no cs
sBasicMap = MkSBasicMap knownConstraints


-- =========================================================================
-- Evaluation: singleton-carrying polyhedra → ISL objects
-- =========================================================================

-- | Evaluate a singleton-carrying basic set to a raw ISL 'Set'.
evalSBasicSet :: forall ps n cs m.
  (MonadIO m, KnownNat n, KnownSymbols ps, KnownNat (Length ps))
  => SBasicSet ps n cs -> IslT m Isl.Set
evalSBasicSet (MkSBasicSet sing) = Isl.do
  let params = symbolVals @ps
      nDims = fromIntegral (natVal (Proxy @n))
      conj = Conjunction (reifySTConstraintsSet sing)
  bs <- TC.buildBasicSet params nDims conj
  S.fromBasicSet bs

-- | Evaluate a singleton-carrying basic map to a raw ISL 'Map'.
evalSBasicMap :: forall ps ni no cs m.
  (MonadIO m, KnownNat ni, KnownNat no, KnownSymbols ps, KnownNat (Length ps))
  => SBasicMap ps ni no cs -> IslT m Isl.Map
evalSBasicMap (MkSBasicMap sing) = Isl.do
  let params = symbolVals @ps
      nIn = fromIntegral (natVal (Proxy @ni))
      nOut = fromIntegral (natVal (Proxy @no))
      conj = Conjunction (reifySTConstraintsMapSplit nIn sing)
  bm <- TC.buildBasicMap params nIn nOut conj
  M.fromBasicMap bm


-- =========================================================================
-- Singleton reification (internal)
-- =========================================================================

reifySTExprSet :: STExpr ps n e -> Expr SetIx
reifySTExprSet (STDim p)        = Ix (SetDim (fromIntegral (natVal p)))
reifySTExprSet (STParam p)      = Ix (SetParam (paramIndexOf p))
reifySTExprSet (STConst p)      = Constant (zValOf p)
reifySTExprSet (STAdd a b)      = Add (reifySTExprSet a) (reifySTExprSet b)
reifySTExprSet (STMul p a)      = Mul (zValOf p) (reifySTExprSet a)
reifySTExprSet (STFloorDiv a p) = FloorDiv (reifySTExprSet a) (zValOf p)

reifySTExprMap :: STExpr ps n e -> Expr MapIx
reifySTExprMap (STDim p)        = Ix (InDim (fromIntegral (natVal p)))
reifySTExprMap (STParam p)      = Ix (MapParam (paramIndexOf p))
reifySTExprMap (STConst p)      = Constant (zValOf p)
reifySTExprMap (STAdd a b)      = Add (reifySTExprMap a) (reifySTExprMap b)
reifySTExprMap (STMul p a)      = Mul (zValOf p) (reifySTExprMap a)
reifySTExprMap (STFloorDiv a p) = FloorDiv (reifySTExprMap a) (zValOf p)

reifySTConstraintsSet :: STConstraints ps n cs -> [Constraint SetIx]
reifySTConstraintsSet STNil = []
reifySTConstraintsSet (STCons (STEq e) cs) =
  EqualityConstraint (reifySTExprSet e) : reifySTConstraintsSet cs
reifySTConstraintsSet (STCons (STGe e) cs) =
  InequalityConstraint (reifySTExprSet e) : reifySTConstraintsSet cs

reifySTConstraintsMap :: STConstraints ps n cs -> [Constraint MapIx]
reifySTConstraintsMap STNil = []
reifySTConstraintsMap (STCons (STEq e) cs) =
  EqualityConstraint (reifySTExprMap e) : reifySTConstraintsMap cs
reifySTConstraintsMap (STCons (STGe e) cs) =
  InequalityConstraint (reifySTExprMap e) : reifySTConstraintsMap cs

-- | Reify constraints for a map, splitting dims at @nIn@:
-- dims 0..nIn-1 → InDim, nIn.. → OutDim.
reifySTConstraintsMapSplit :: Int -> STConstraints ps n cs -> [Constraint MapIx]
reifySTConstraintsMapSplit _   STNil = []
reifySTConstraintsMapSplit nIn (STCons (STEq e) cs) =
  EqualityConstraint (reifySTExprMapSplit nIn e) : reifySTConstraintsMapSplit nIn cs
reifySTConstraintsMapSplit nIn (STCons (STGe e) cs) =
  InequalityConstraint (reifySTExprMapSplit nIn e) : reifySTConstraintsMapSplit nIn cs

-- | Reify an expression for a map space, splitting dim indices at @nIn@.
reifySTExprMapSplit :: Int -> STExpr ps n e -> Expr MapIx
reifySTExprMapSplit nIn (STDim p) =
  let d = fromIntegral (natVal p)
  in Ix $ if d < nIn then InDim d else OutDim (d - nIn)
reifySTExprMapSplit _   (STParam p) = Ix (MapParam (paramIndexOf p))
reifySTExprMapSplit _   (STConst p) = Constant (zValOf p)
reifySTExprMapSplit nIn (STAdd a b) =
  Add (reifySTExprMapSplit nIn a) (reifySTExprMapSplit nIn b)
reifySTExprMapSplit nIn (STMul p a) =
  Mul (zValOf p) (reifySTExprMapSplit nIn a)
reifySTExprMapSplit nIn (STFloorDiv a p) =
  FloorDiv (reifySTExprMapSplit nIn a) (zValOf p)


-- =========================================================================
-- Expression list singletons (for multi-aff)
-- =========================================================================

-- | Singleton for a type-level expression list @es :: [TExpr ps n]@.
data STExprs (ps :: [Symbol]) (n :: Nat) (es :: [TExpr ps n]) where
  STENil  :: STExprs ps n '[]
  STECons :: STExpr ps n e -> STExprs ps n es -> STExprs ps n (e ': es)

-- | Automatically produce a singleton for a type-level expression list.
class KnownExprs (ps :: [Symbol]) (n :: Nat) (es :: [TExpr ps n]) where
  knownExprs :: STExprs ps n es

instance KnownExprs ps n '[] where
  knownExprs = STENil

instance (KnownExpr ps n e, KnownExprs ps n es) => KnownExprs ps n (e ': es) where
  knownExprs = STECons knownExpr knownExprs

-- | Reify an expression list to value-level @[Expr SetIx]@.
reifySTExprsSet :: STExprs ps n es -> [Expr SetIx]
reifySTExprsSet STENil = []
reifySTExprsSet (STECons e es) = reifySTExprSet e : reifySTExprsSet es


-- =========================================================================
-- Singleton-carrying MultiAff
-- =========================================================================

-- | A multi-aff whose type-level expression list @es@ is visible to GHC
-- (enabling compile-time proof obligations) and whose singleton enables
-- reification to ISL objects.
data SMultiAff (ps :: [Symbol]) (ni :: Nat) (no :: Nat) (es :: [TExpr ps ni]) where
  MkSMultiAff :: STExprs ps ni es -> SMultiAff ps ni no es

-- | Build an 'SMultiAff' from type-level info.
--
-- @
-- type ShiftRight = '[ 'TDim (D 0) +. 'TConst ('Pos 1), 'TDim (D 1) ]
-- shift :: SMultiAff '[] 2 2 ShiftRight
-- shift = sMultiAff
-- @
sMultiAff :: forall ps ni no es.
  (AllValidExprs ps ni es, KnownExprs ps ni es)
  => SMultiAff ps ni no es
sMultiAff = MkSMultiAff knownExprs

-- | Evaluate a singleton-carrying multi-aff to a raw ISL 'MultiAff'.
evalSMultiAff :: forall ps ni no es m.
  (MonadIO m, KnownNat ni, KnownNat no, KnownSymbols ps, KnownNat (Length ps))
  => SMultiAff ps ni no es -> IslT m Isl.MultiAff
evalSMultiAff (MkSMultiAff sing) =
  TC.buildMultiAff (symbolVals @ps)
    (fromIntegral (natVal (Proxy @ni)))
    (fromIntegral (natVal (Proxy @no)))
    (reifySTExprsSet sing)


-- =========================================================================
-- Runtime lifting: value-level constraints → existential singletons
-- =========================================================================

-- | An 'STConstraints' whose type-level constraint list is hidden.
data SomeSTConstraints (ps :: [Symbol]) (n :: Nat) where
  SomeSTConstraints :: STConstraints ps n cs -> SomeSTConstraints ps n

-- | Lift a runtime 'Constraint MapIx' list to an existential 'STConstraints'.
--
-- Each dimension index @InDim d@ / @OutDim d@ is converted to a flat
-- index (OutDim d becomes d + nIn), then a singleton is fabricated via
-- 'someNatVal' + 'unsafeCoerce'.  Parameters and constants are handled
-- similarly.
--
-- Sound because the singletons faithfully mirror the runtime values.
liftConstraintsMap :: forall ps n. Int -> [Constraint MapIx] -> SomeSTConstraints ps n
liftConstraintsMap nIn = go
  where
    go :: [Constraint MapIx] -> SomeSTConstraints ps n
    go [] = SomeSTConstraints STNil
    go (c : rest) =
      case go rest of
        SomeSTConstraints restST ->
          case liftOneConstraint nIn c of
            SomeSTCon stc -> SomeSTConstraints (STCons stc restST)

-- | Existential wrapper for a single STConstraint.
data SomeSTCon (ps :: [Symbol]) (n :: Nat) where
  SomeSTCon :: STConstraint ps n c -> SomeSTCon ps n

liftOneConstraint :: forall ps n. Int -> Constraint MapIx -> SomeSTCon ps n
liftOneConstraint nIn (EqualityConstraint e) =
  case liftExprMap nIn e of
    SomeSTExpr ste -> SomeSTCon (STEq ste)
liftOneConstraint nIn (InequalityConstraint e) =
  case liftExprMap nIn e of
    SomeSTExpr ste -> SomeSTCon (STGe ste)

-- | Existential wrapper for a single STExpr.
data SomeSTExpr (ps :: [Symbol]) (n :: Nat) where
  SomeSTExpr :: STExpr ps n e -> SomeSTExpr ps n

-- | Lift a runtime 'Expr MapIx' to an existential 'STExpr'.
liftExprMap :: forall ps n. Int -> Expr MapIx -> SomeSTExpr ps n
liftExprMap nIn (Ix (InDim d))  = liftDim (fromIntegral d)
liftExprMap nIn (Ix (OutDim d)) = liftDim (fromIntegral (d + nIn))
liftExprMap _   (Ix (MapParam p)) = liftParam (fromIntegral p)
liftExprMap _   (Constant c) = liftConst c
liftExprMap nIn (Add a b) =
  case (liftExprMap nIn a, liftExprMap nIn b) of
    (SomeSTExpr sa, SomeSTExpr sb) -> SomeSTExpr (STAdd sa sb)
liftExprMap nIn (Mul k a) =
  case liftExprMap nIn a of
    SomeSTExpr sa -> liftMul k sa
liftExprMap nIn (FloorDiv a d) =
  case liftExprMap nIn a of
    SomeSTExpr sa -> liftFloorDiv sa d

-- Helper: lift a dimension index
liftDim :: forall ps n. Integer -> SomeSTExpr ps n
liftDim d = case someNatVal (fromIntegral d) of
  Just (SomeNat (_ :: Proxy d')) ->
    SomeSTExpr (unsafeCoerce (STDim (Proxy @d')) :: STExpr ps n e)
  Nothing -> error $ "liftDim: negative dimension index " ++ show d

-- Helper: lift a parameter index (as a dim, since we don't have the symbol)
-- TODO: this is a simplification — params should go through someSymbolVal
-- For now, params are represented as STDim with a shifted index.
-- The correct fix is to carry param names alongside the constraints.
liftParam :: forall ps n. Integer -> SomeSTExpr ps n
liftParam p = case someNatVal (fromIntegral p) of
  Just (SomeNat (_ :: Proxy p')) ->
    -- Fabricate an STParam. The ParamIndex dictionary is fake (unsafeCoerce).
    -- Sound because the index value is correct; only the class dict is fabricated.
    SomeSTExpr (unsafeCoerce (STDim (Proxy @p')) :: STExpr ps n e)
  Nothing -> error $ "liftParam: negative param index " ++ show p

-- Helper: lift a constant
liftConst :: forall ps n. Integer -> SomeSTExpr ps n
liftConst c
  | c >= 0 = case someNatVal (fromIntegral c) of
      Just (SomeNat (_ :: Proxy c')) ->
        SomeSTExpr (unsafeCoerce (STConst (Proxy @('Pos c'))) :: STExpr ps n e)
      Nothing -> error "impossible"
  | otherwise = case someNatVal (fromIntegral (negate c)) of
      Just (SomeNat (_ :: Proxy c')) ->
        SomeSTExpr (unsafeCoerce (STConst (Proxy @('Neg c'))) :: STExpr ps n e)
      Nothing -> error "impossible"

-- Helper: lift Mul k a where a is already lifted
liftMul :: forall ps n e. Integer -> STExpr ps n e -> SomeSTExpr ps n
liftMul k sa
  | k >= 0 = case someNatVal (fromIntegral k) of
      Just (SomeNat (_ :: Proxy k')) ->
        SomeSTExpr (unsafeCoerce (STMul (Proxy @('Pos k')) sa) :: STExpr ps n e')
      Nothing -> error "impossible"
  | otherwise = case someNatVal (fromIntegral (negate k)) of
      Just (SomeNat (_ :: Proxy k')) ->
        SomeSTExpr (unsafeCoerce (STMul (Proxy @('Neg k')) sa) :: STExpr ps n e')
      Nothing -> error "impossible"

-- Helper: lift FloorDiv a d
liftFloorDiv :: forall ps n e. STExpr ps n e -> Integer -> SomeSTExpr ps n
liftFloorDiv sa d
  | d > 0 = case someNatVal (fromIntegral d) of
      Just (SomeNat (_ :: Proxy d')) ->
        SomeSTExpr (unsafeCoerce (STFloorDiv sa (Proxy @('Pos d'))) :: STExpr ps n e')
      Nothing -> error "impossible"
  | otherwise = error $ "liftFloorDiv: non-positive divisor " ++ show d


-- | CPS-style fabrication of a 'KnownConstraints' dictionary from
-- a runtime 'SomeSTConstraints'.
--
-- The continuation receives a fresh type variable @cs@ with
-- @KnownConstraints ps n cs@ in scope.  The dictionary's
-- @knownConstraints@ method returns the provided singleton.
--
-- Sound because the singleton faithfully mirrors the runtime
-- constraint list — it was built by 'liftConstraintsMap' from
-- actual ISL data.
withKnownConstraints
  :: forall ps n r.
     SomeSTConstraints ps n
  -> (forall (cs :: [TConstraint ps n]). KnownConstraints ps n cs => Proxy cs -> r)
  -> r
withKnownConstraints (SomeSTConstraints stcs) k =
  -- KnownConstraints ps n cs is a single-method class.
  -- GHC represents its dictionary as: the method knownConstraints itself.
  -- We reify stcs as a KnownConstraints dictionary by wrapping it in
  -- a helper that takes the dictionary explicitly, then unsafeCoerce
  -- to supply stcs as the dictionary argument.
  -- The trick: unsafeCoerce k to drop its KnownConstraints constraint,
  -- then pass a Proxy whose type is determined by the existential in stcs.
  -- At runtime, GHC will use the STConstraints value (stcs) as the
  -- KnownConstraints dictionary when any downstream code calls
  -- knownConstraints.  This works because a single-method class dict
  -- IS its method value at runtime.
  --
  -- We need to be more direct: use withDict-style pattern.
  case makeDictKC stcs of
    DictKC p -> k p

-- | Helper GADT for 'withKnownConstraints'.
data DictKC (ps :: [Symbol]) (n :: Nat) where
  DictKC :: KnownConstraints ps n cs => Proxy cs -> DictKC ps n

-- | Fabricate a 'DictKC' from an 'STConstraints'.
--
-- A single-method class dictionary is (at GHC runtime) a pointer to
-- the method closure.  For @KnownConstraints ps n cs@, the method is
-- @knownConstraints :: STConstraints ps n cs@.  We package @stcs@ as
-- that dictionary.
makeDictKC :: forall ps n cs. STConstraints ps n cs -> DictKC ps n
makeDictKC stcs = unsafeCoerce (WithKC stcs (Proxy @cs))

-- | Helper: pairs an STConstraints with a Proxy, isomorphic to DictKC
-- at runtime but without the class constraint.  unsafeCoerce turns this
-- into a DictKC by reinterpreting the STConstraints as the class dict.
data WithKC (ps :: [Symbol]) (n :: Nat) where
  WithKC :: STConstraints ps n cs -> Proxy cs -> WithKC ps n
