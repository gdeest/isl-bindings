{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Surface → Core.V2 elaborator.
--
-- @Alpha.Surface.system@ produces an 'Alpha.Core.System' directly —
-- named dimension labels are compiled to positional @D 0@, @D 1@,
-- @D 2@ at the type level, so the Surface and Core pattern synonyms
-- share a data representation.  The elaborator is therefore a thin
-- wrapper over 'Alpha.Core.Bridge.toV2': it re-exports the CPS
-- signature the user-facing API wants, fresh-skolemizes every
-- declaration/domain/map, and runs the ISL-backed token checkers to
-- produce V2 proof tokens.
--
-- See 'Alpha.Core.Bridge' for the walker's internals and the
-- design of the token-fabrication trust boundary.
module Alpha.Surface.Elaborate
  ( elaborate
  , ElabError(..)
  ) where

import qualified Alpha.Core        as Legacy
import qualified Alpha.Core.V2     as V2
import           Alpha.Core.Bridge (toV2)
import           Alpha.Core.Tokens (ElabError(..))
import           Isl.Typed.Params   (KnownSymbols)


-- | Elaborate a Surface system to V2 Core.  The continuation is
-- rank-N in the system skolem @sys@; users never name it.
--
-- The element type @a@ is an explicit type argument — the caller
-- chooses it to match the system's scalar (e.g., @Double@).  Phase B
-- will thread @a@ out of the system's declarations automatically.
elaborate
  :: forall ps pctx ins outs locs a r.
     KnownSymbols ps
  => Legacy.System ps pctx ins outs locs
  -> (forall sys. Either ElabError (V2.System sys a) -> r)
  -> r
elaborate = toV2
