module Isl.HighLevel.Context
  ( runIsl
  , Isl
  , Ur(..)
  , Borrow(..)
  , Consumable(..)
  , Dupable(..)
  , freeM
  , borrowPure
  ) where

import Isl.Monad (runIsl, Isl, Ur(..), freeM)
import Isl.Types (Borrow(..), Consumable(..), Dupable(..))
import Isl.Linear (borrowPure)
