-- | The ISL monad. Carries an ISL context through a computation and
-- ensures cleanup. 'IslT' has no standard 'Monad' instance — compose
-- ISL actions exclusively via @Isl.do@ ('Isl.Linear' QualifiedDo).
--
-- The 'IslT' constructor is hidden. Use 'Isl.Linear' to compose
-- ISL operations, 'runIslT'/'runIsl' to execute, and 'withIslCtx'
-- for externally-managed contexts (e.g. GHC plugins).
module Isl.Monad
  ( IslT       -- opaque, no constructor
  , Isl
  , Ur(..)
  , Both(..)
  , runIslT
  , runIsl
  , withIslCtx
  , unsafeIslFromIO
  , checkIslError
  ) where

import Isl.Monad.Internal
  ( IslT, Isl, Ur(..), Both(..)
  , runIslT, runIsl, withIslCtx, unsafeIslFromIO, checkIslError )
