{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module — resource management typeclasses with methods exposed.
-- Not importable from outside the package (listed in other-modules).
-- Public code should use 'query', 'query_', 'freeM', and 'dup' from
-- "Isl.Linear" instead of calling 'consume' or 'borrow' directly.
module Isl.Types.Internal
  ( Consumable(..)
  , Borrow(..)
  , Dupable(..)
  ) where

class Consumable a where
  consume :: a %1 -> ()

class Borrow owned ref | owned -> ref where
  borrow :: owned %1 -> (ref -> a) -> (a, owned)

class Consumable a => Dupable a where
  dup :: a %1 -> (a, a)
