-- | Imperative polyhedral DSL for ISL.
--
-- This module re-exports everything needed to write polyhedral programs.
--
-- @
-- {-\# LANGUAGE DataKinds \#-}
-- {-\# LANGUAGE TypeApplications \#-}
-- module Main where
-- import Isl.DSL
--
-- jacobi1D :: Program '[\"N\", \"T\"]
-- jacobi1D = 'program' $ do
--   old <- 'array' \"old\" ['range' 0 ('param' \@\"N\" + 1)]
--   new <- 'array' \"new\" ['range' 0 ('param' \@\"N\" + 1)]
--   'rescheduled' ('skew' \@\"t\" \@\"i\" 1 >> 'parallel' \@\"t\") $
--     'for_' \@\"t\" ('range' 1 ('param' \@\"T\")) $ \\t ->
--       'for_' \@\"i\" ('range' 1 ('param' \@\"N\")) $ \\i ->
--         new'!['ix' i] '<==' (old'!['ix' i-1] + old'!['ix' i] + old'!['ix' i+1]) / 3
-- @
module Isl.DSL
  ( -- * Types
    module Isl.DSL.Types
    -- * Builder
  , module Isl.DSL.Builder
    -- * Compilation
  , module Isl.DSL.Compile
    -- * Execution
  , module Isl.DSL.Execute
    -- * Verification
  , module Isl.DSL.Verify
  ) where

import Isl.DSL.Types
import Isl.DSL.Builder
import Isl.DSL.Compile
import Isl.DSL.Execute
import Isl.DSL.Verify
