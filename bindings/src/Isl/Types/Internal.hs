{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

-- | Internal module — resource management typeclasses with methods exposed.
-- Not importable from outside the package (listed in other-modules).
-- Public code should use 'query', 'query_', 'freeM', and 'dup' from
-- "Isl.Linear" instead of calling 'consume' or 'borrow' directly.
module Isl.Types.Internal
  ( Consumable(..)
  , Borrow(..)
  , Dupable(..)
  ) where

import Data.Kind (Type)

class Consumable a where
  consume :: a %1 -> IO ()

-- | Borrow an owned ISL object to obtain a region-indexed reference.
-- The rank-2 callback forces the result type @a@ to be independent of
-- the skolem region @s@, so the ref cannot escape.
class Borrow owned (ref :: Type -> Type) | owned -> ref where
  borrow :: owned %1 -> (forall s. ref s -> a) -> (a, owned)

class Consumable a => Dupable a where
  dup :: a %1 -> IO (a, a)
