{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Singletons for reifying type-level polyhedral constraints to values.
--
-- These let you go from @'TBasicSet' ps n cs@ to a value-level
-- @PConjunction ps n@ that can be handed to ISL.
module Isl.TypeLevel.Sing
  ( -- * Reification of type-level integers
    KnownZ(..)
    -- * Parameter name → index mapping
  , ParamIndex(..)
    -- * Reification of type-level expressions
  , ReifyExpr(..)
    -- * Reification of type-level constraints
  , ReifyTConstraint(..)
  , ReifyTConstraints(..)
    -- * Full reification to ISL-ready types
  , reifyBasicSet
  , reifyBasicMap
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (Nat, KnownNat, natVal, Symbol, KnownSymbol, symbolVal, type (+))

import Isl.TypeLevel.Expr
import Isl.TypeLevel.Constraint (TConstraint(..), AllValid)
import Isl.HighLevel.Constraints (Expr(..), Constraint(..), Conjunction(..), SetIx(..), MapIx(..))
import Isl.HighLevel.Pure (PConjunction(..), PMapConjunction(..))
import Isl.HighLevel.Params (KnownSymbols, symbolVals)

-- * Signed integer reification

class KnownZ (z :: Z) where
  zVal :: Integer

instance KnownNat n => KnownZ ('Pos n) where
  zVal = fromIntegral (natVal (Proxy @n))

instance KnownNat n => KnownZ ('Neg n) where
  zVal = negate (fromIntegral (natVal (Proxy @n)))


-- * Expression reification (generic over the index type)

-- | Reify a type-level expression to a value-level 'Expr'.
--
-- The @ix@ parameter determines what kind of index variables we produce
-- ('SetIx' for sets, 'MapIx' for maps).
class ReifyExpr (e :: TExpr) ix where
  reifyExpr :: Expr ix

-- Dimension → SetDim for sets
instance KnownNat d => ReifyExpr ('TDim d) SetIx where
  reifyExpr = Ix (SetDim (fromIntegral (natVal (Proxy @d))))

-- Dimension → InDim for maps (dimensions are input dims by default;
-- output dims need explicit shifting, handled separately)
instance KnownNat d => ReifyExpr ('TDim d) MapIx where
  reifyExpr = Ix (InDim (fromIntegral (natVal (Proxy @d))))

-- Parameter by name → for reification we need the parameter's positional
-- index within the parameter list. This is resolved externally (by the
-- plugin or by the caller providing the right context). For now, we
-- provide instances that work with a 'ParamIndex' class that maps
-- Symbol → Nat position.

-- | Maps a parameter name to its positional index in a parameter list.
-- Users can derive this from 'KnownSymbols', or the plugin can solve it.
class ParamIndex (s :: Symbol) where
  paramIndex :: Int

instance ParamIndex s => ReifyExpr ('TParam s) SetIx where
  reifyExpr = Ix (SetParam (paramIndex @s))

instance ParamIndex s => ReifyExpr ('TParam s) MapIx where
  reifyExpr = Ix (MapParam (paramIndex @s))

instance KnownZ z => ReifyExpr ('TConst z) ix where
  reifyExpr = Constant (zVal @z)

instance (ReifyExpr a ix, ReifyExpr b ix) => ReifyExpr ('TAdd a b) ix where
  reifyExpr = Add (reifyExpr @a @ix) (reifyExpr @b @ix)

instance (KnownZ k, ReifyExpr a ix) => ReifyExpr ('TMul k a) ix where
  reifyExpr = Mul (zVal @k) (reifyExpr @a @ix)


-- * Constraint reification

class ReifyTConstraint (c :: TConstraint) ix where
  reifyTConstraint :: Constraint ix

instance ReifyExpr e ix => ReifyTConstraint ('TEq e) ix where
  reifyTConstraint = EqualityConstraint (reifyExpr @e @ix)

instance ReifyExpr e ix => ReifyTConstraint ('TGe e) ix where
  reifyTConstraint = InequalityConstraint (reifyExpr @e @ix)


-- * Constraint list reification

class ReifyTConstraints (cs :: [TConstraint]) ix where
  reifyTConstraints :: [Constraint ix]

instance ReifyTConstraints '[] ix where
  reifyTConstraints = []

instance (ReifyTConstraint c ix, ReifyTConstraints cs ix) =>
         ReifyTConstraints (c ': cs) ix where
  reifyTConstraints = reifyTConstraint @c @ix : reifyTConstraints @cs @ix


-- * Full reification

-- | Reify a type-level basic set to a value-level 'PConjunction'.
--
-- @
-- type MySet = TBasicSet '["N"] 2 '[...]
-- mySet :: PConjunction '["N"] 2
-- mySet = reifyBasicSet @'["N"] @2 @'[...]
-- @
reifyBasicSet
  :: forall ps n cs. (AllValid ps n cs, ReifyTConstraints cs SetIx)
  => PConjunction ps n
reifyBasicSet = PConjunction (Conjunction (reifyTConstraints @cs @SetIx))

-- | Reify a type-level basic map to a value-level 'PMapConjunction'.
reifyBasicMap
  :: forall ps ni no cs. (AllValid ps (ni + no) cs, ReifyTConstraints cs MapIx)
  => PMapConjunction ps ni no
reifyBasicMap = PMapConjunction (Conjunction (reifyTConstraints @cs @MapIx))
