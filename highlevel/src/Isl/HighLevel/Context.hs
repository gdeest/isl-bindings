module Isl.HighLevel.Context
  ( runIslT
  , runIsl
  , IslT
  , Isl
  , Ur(..)
  , Borrow(..)
  , Consumable(..)
  , Dupable(..)
  , freeM
  , borrowPure
  ) where

import Isl.Monad (runIslT, runIsl, IslT, Isl, Ur(..), freeM)
import Isl.Types (Borrow(..), Consumable(..), Dupable(..))
import Isl.Linear (borrowPure)
