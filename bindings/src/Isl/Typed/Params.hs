{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type-level machinery for tracking ISL parameter names.
--
-- ISL parameters are named (e.g. @N@, @M@) and operations on sets/maps
-- with different parameter spaces merge them by name. This module provides
-- type families and classes to track this at the type level.
--
-- Parameter lists are kept in sorted (lexicographic) order, matching
-- ISL's internal canonical ordering.
module Isl.Typed.Params
  ( -- * Type families
    Union
  , Length
  , IsSubsetOf
    -- * Runtime recovery
  , KnownSymbols(..)
  ) where

import Data.Kind (Constraint)
import Data.Proxy
import GHC.TypeLits

-- | Sorted merge (union) of two symbol lists.
type family Union (xs :: [Symbol]) (ys :: [Symbol]) :: [Symbol] where
  Union '[] ys = ys
  Union xs '[] = xs
  Union (x ': xs) (y ': ys) = UnionCase (CmpSymbol x y) x xs y ys

type family UnionCase (ord :: Ordering) (x :: Symbol) (xs :: [Symbol]) (y :: Symbol) (ys :: [Symbol]) :: [Symbol] where
  UnionCase 'LT x xs y ys = x ': Union xs (y ': ys)
  UnionCase 'EQ x xs _ ys = x ': Union xs ys
  UnionCase 'GT x xs y ys = y ': Union (x ': xs) ys

-- | Length of a type-level list.
type family Length (xs :: [k]) :: Nat where
  Length '[] = 0
  Length (_ ': xs) = 1 + Length xs

-- | Constraint that @xs@ is a subset of @ys@ (both sorted).
type family IsSubsetOf (xs :: [Symbol]) (ys :: [Symbol]) :: Constraint where
  IsSubsetOf '[] _ = ()
  IsSubsetOf (x ': xs) (y ': ys) = IsSubsetOfCase (CmpSymbol x y) x xs y ys

type family IsSubsetOfCase (ord :: Ordering) (x :: Symbol) (xs :: [Symbol]) (y :: Symbol) (ys :: [Symbol]) :: Constraint where
  IsSubsetOfCase 'LT x _ _ _ = TypeError ('Text "Parameter " ':<>: 'ShowType x ':<>: 'Text " not found in target parameter space")
  IsSubsetOfCase 'EQ _ xs _ ys = IsSubsetOf xs ys
  IsSubsetOfCase 'GT x xs _ ys = IsSubsetOf (x ': xs) ys

-- | Recover parameter names at runtime from a type-level symbol list.
class KnownSymbols (ss :: [Symbol]) where
  symbolVals :: [String]

instance KnownSymbols '[] where
  symbolVals = []

instance (KnownSymbol s, KnownSymbols ss) => KnownSymbols (s ': ss) where
  symbolVals = symbolVal (Proxy @s) : symbolVals @ss
