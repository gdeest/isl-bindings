-- | OpenMP pragma emission for parallel dimensions.
module Isl.Infer.Codegen.OpenMP
  ( ompPragma
  , ompParallelFor
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

-- | Generate an OpenMP pragma line if the given dimension index
-- is in the parallel set. Returns 'Nothing' if not parallel.
ompPragma :: Set Int -> Int -> Maybe String
ompPragma parallelDims dimIdx
  | dimIdx `Set.member` parallelDims = Just "#pragma omp parallel for schedule(static)"
  | otherwise = Nothing

-- | Like 'ompPragma' but with a collapse count for adjacent parallel loops.
ompParallelFor :: Int -> String
ompParallelFor 1 = "#pragma omp parallel for schedule(static)"
ompParallelFor n = "#pragma omp parallel for collapse(" ++ show n ++ ") schedule(static)"
