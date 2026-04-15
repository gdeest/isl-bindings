{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Schedule representation for Alpha systems.
--
-- = Typed surface
--
-- The preferred API derives the dimension count from the system's
-- declarations:
--
-- @
-- s = scheduling $ do
--   sched \@\"C\" \@MatmulDecls $ \\n -> identity n
--   sched \@\"D\" \@FWDecls    $ \\n -> embedAt 0 (identity n)
-- @
--
-- The callback receives the equation's iteration dimension count,
-- so 'identity', 'tile', etc. always get the right value.
module Alpha.Schedule
  ( -- * Types
    EqSchedule(..)
  , Schedule(..)
    -- * Builder
  , ScheduleBuilder
  , scheduling
    -- * Typed combinator
  , sched
    -- * Untyped combinator (escape hatch)
  , schedule
    -- * Schedule combinators (re-exports)
  , S.ScheduleDef(..)
  , S.identity
  , S.tile
  , S.skew
  , S.interchange
  , S.shift
  , embedAt
  ) where

import Control.Monad.State.Strict (State, modify', execState)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)
import qualified Alpha.Polyhedral.Schedule as S
import Isl.Typed.Constraints (Expr(..), MapIx(..))

import Alpha.Core (VarDecl(..), Lookup, DeclDims)


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Types
-- ═══════════════════════════════════════════════════════════════════════

data EqSchedule = EqSchedule
  { esName  :: !String
  , esNIter :: !Int
  , esNTime :: !Int
  , esDef   :: !S.ScheduleDef
  } deriving (Show)

newtype Schedule = Schedule { schedEntries :: Map String EqSchedule }
  deriving (Show)


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Builder
-- ═══════════════════════════════════════════════════════════════════════

type ScheduleBuilder a = State (Map String EqSchedule) a

scheduling :: ScheduleBuilder () -> Schedule
scheduling = Schedule . flip execState Map.empty


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Typed combinator
-- ═══════════════════════════════════════════════════════════════════════

-- | Assign a schedule to a named variable.  The callback receives
-- the equation's iteration dimension count (derived from the
-- declaration list at the type level).
--
-- @
-- sched \@\"C\" \@MatmulDecls $ \\n -> identity n
-- sched \@\"u\" \@H3Decls    $ \\n -> identity n
-- sched \@\"D\" \@FWDecls    $ \\n -> embedAt 0 (identity n)
-- @
sched
  :: forall (name :: Symbol) {ps} (decls :: [VarDecl ps]) {decl}.
     ( decl ~ Lookup name decls
     , KnownSymbol name
     , KnownNat (DeclDims decl)
     )
  => (Int -> S.ScheduleDef) -> ScheduleBuilder ()
sched mkDef =
  let nIter = fromIntegral (natVal (Proxy @(DeclDims decl)))
  in schedule (symbolVal (Proxy @name)) nIter (mkDef nIter)


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Untyped combinator
-- ═══════════════════════════════════════════════════════════════════════

schedule :: String -> Int -> S.ScheduleDef -> ScheduleBuilder ()
schedule name nIter def = modify' (Map.insert name eq)
  where
    eq = EqSchedule
      { esName  = name
      , esNIter = nIter
      , esNTime = length (S.schedExprs def)
      , esDef   = def
      }


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Schedule combinators
-- ═══════════════════════════════════════════════════════════════════════

-- | Prepend a constant phase dimension: @[phase, d0, d1, ...]@.
embedAt :: Int -> S.ScheduleDef -> S.ScheduleDef
embedAt phase (S.ScheduleDef es) =
  S.ScheduleDef (Constant (fromIntegral phase) : es)
