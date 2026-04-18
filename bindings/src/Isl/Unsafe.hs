{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unsafe escape hatch for code that cannot use 'IslT' (e.g. GHC plugins
-- operating in pure IO via unsafePerformIO). Prefer 'Isl.Linear' for
-- all normal usage.
--
-- These functions bypass the linear monad's sequencing guarantees.
-- Use 'Isl.Monad.withIslCtx' + 'Isl.Linear' wherever possible;
-- reserve this module for persistent ISL objects that outlive any
-- single scoped computation.
module Isl.Unsafe
  ( consume
  , borrow
  , dup
  ) where

import Isl.Types.Internal (Consumable(consume), Borrow(borrow), Dupable(dup))
