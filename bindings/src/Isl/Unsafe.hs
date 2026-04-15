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
  , consumeIO
  ) where

import Control.Exception (evaluate)
import Unsafe.Coerce (unsafeCoerce)
import Isl.Types.Internal (Consumable(consume), Borrow(borrow), Dupable(dup))

-- | Consume an ISL object in IO, sequenced (not lazy).
-- Safer than bare 'consume' which uses unsafePerformIO and can be deferred.
consumeIO :: forall a. Consumable a => a %1 -> IO ()
consumeIO = unsafeCoerce go
  where
    go :: Consumable a => a -> IO ()
    go x = evaluate (consume x)
