-- | Pretty-printing for multi-statement scanners.
--
-- Produces merged loop nest output in CLooG-like style: shared outer
-- loops are emitted once, and statements are sequenced or guarded
-- where their iteration spaces diverge.
--
-- @
-- for t0 = 0 to N-1:
--   for t1 = 0 to M-1:
--     S0(i=t0, j=t1)
--     for t3 = 0 to K-1:
--       S1(i=t0, j=t1, k=t3)
-- @
module Isl.Scan.PrettyMulti
  ( MergedAST(..)
  , buildMergedAST
  , prettyMergedAST
  , prettyMultiScanner
  ) where

import Data.List (intercalate, sortBy, groupBy)
import Data.Ord (comparing)

import Isl.Scan.Types (LoopLevel(..), AffineBound(..))
import Isl.Scan.Pretty (prettyBound)
import Isl.Scan.Multi (MultiScanner(..), ScheduledStatement(..), ULoopNest(..), StmtInverse(..))

-- | Merged AST for multi-statement loop nests.
data MergedAST
  = MLoop String [AffineBound] [AffineBound] [MergedAST]
    -- ^ Shared loop: dim name, lower bounds, upper bounds, body
  | MLet String AffineBound [MergedAST]
    -- ^ Equality-constrained dimension: dim name, value, body
  | MStmtBody String [String]
    -- ^ Statement body: stmt name, argument expressions (e.g. ["i=t0", "j=t1"])
  | MSequence [MergedAST]
    -- ^ Sequential composition of blocks
  deriving (Show)

-- | A statement with its loop levels, used during AST construction.
data StmtEntry = StmtEntry
  { seName   :: String
  , seLevels :: [LoopLevel]
  , seInverse :: StmtInverse
  }

-- | Build a merged AST from a multi-scanner's statements.
-- Uses the first loop nest per statement (if a statement has multiple
-- disjuncts, each is treated as a separate entry).
buildMergedAST :: [String] -> MultiScanner nParams -> MergedAST
buildMergedAST timeDimNames (MultiScanner stmts) =
  let entries = [ StmtEntry (ssName s) (ulnLevels nest) (ssInverse s)
                | s <- stmts, nest <- ssLoopNests s ]
      nTime = case entries of
                (e:_) -> length (seLevels e)
                []    -> 0
  in mergeAtDim timeDimNames nTime entries 0

mergeAtDim :: [String] -> Int -> [StmtEntry] -> Int -> MergedAST
mergeAtDim dimNames nTime entries dim
  | dim >= nTime || null entries =
      -- Base case: emit statement bodies
      MSequence [MStmtBody (seName e) (inverseArgs dimNames (seInverse e))
                | e <- entries]
  | otherwise =
      let -- Partition into equality-constrained ("pinned") and range-constrained
          (pinned, ranging) = partitionByEquality entries dim
      in case (pinned, ranging) of
           -- Case 1: All ranging with same bounds → shared loop
           ([], _) | allSameBoundsAt dim ranging ->
             let level = (seLevels (head ranging)) !! dim
             in MLoop (safeDimName dimNames dim)
                      (llLowerBounds level)
                      (llUpperBounds level)
                      [mergeAtDim dimNames nTime ranging (dim + 1)]

           -- Case 2: All pinned → sequence by equality value
           (_, []) ->
             let grouped = groupByEqualityValue pinned dim
                 blocks = map (\grp ->
                   let eqBound = case llEquality ((seLevels (head grp)) !! dim) of
                                   Just b  -> b
                                   Nothing -> error "prettyMulti: expected equality"
                   in MLet (safeDimName dimNames dim) eqBound
                           [mergeAtDim dimNames nTime grp (dim + 1)]
                   ) grouped
             in MSequence blocks

           -- Case 3: Mixed (some pinned, some ranging) → shared loop,
           -- pinned stmts become let-bindings inside, ranging stmts continue
           _ ->
             -- Compute enclosing bounds from all ranging statements
             let rangeLevels = map (\e -> (seLevels e) !! dim) ranging
                 allLowers = concatMap llLowerBounds rangeLevels
                 allUppers = concatMap llUpperBounds rangeLevels
                 -- Build inner: ranging stmts recurse, pinned stmts emit at their value
                 innerRanging = mergeAtDim dimNames nTime ranging (dim + 1)
                 innerPinned = map (\grp ->
                   let eqBound = case llEquality ((seLevels (head grp)) !! dim) of
                                   Just b  -> b
                                   Nothing -> error "prettyMulti: expected equality"
                   in MLet (safeDimName dimNames dim) eqBound
                           [mergeAtDim dimNames nTime grp (dim + 1)]
                   ) (groupByEqualityValue pinned dim)
             in MLoop (safeDimName dimNames dim)
                      allLowers allUppers
                      [MSequence (innerPinned ++ [innerRanging])]

partitionByEquality :: [StmtEntry] -> Int -> ([StmtEntry], [StmtEntry])
partitionByEquality entries dim =
  let isPinned e = case llEquality ((seLevels e) !! dim) of
                     Just _  -> True
                     Nothing -> False
  in (filter isPinned entries, filter (not . isPinned) entries)

allSameBoundsAt :: Int -> [StmtEntry] -> Bool
allSameBoundsAt _ []  = True
allSameBoundsAt _ [_] = True
allSameBoundsAt dim entries =
  let levels = map (\e -> (seLevels e) !! dim) entries
      ref = head levels
  in all (\l -> llLowerBounds l == llLowerBounds ref
             && llUpperBounds l == llUpperBounds ref) (tail levels)

groupByEqualityValue :: [StmtEntry] -> Int -> [[StmtEntry]]
groupByEqualityValue entries dim =
  let getVal e = case llEquality ((seLevels e) !! dim) of
                   Just b  -> abConstant b  -- simple case: constant equality
                   Nothing -> 0
      sorted = sortBy (comparing getVal) entries
  in groupBy (\a b -> getVal a == getVal b) sorted

inverseArgs :: [String] -> StmtInverse -> [String]
inverseArgs dimNames (StmtInverse nOrig bounds) =
  [ "d" ++ show i ++ "=" ++ prettyBound [] dimNames b
  | (i, b) <- zip [0..] bounds
  ]

safeDimName :: [String] -> Int -> String
safeDimName names i
  | i >= 0 && i < length names = names !! i
  | otherwise = "t" ++ show i

-- * Pretty-printing

-- | Pretty-print a multi-scanner as merged pseudo-code loop nests.
--
-- @timeDimNames@ provides names for the time dimensions (e.g. @["t0","t1","t2","t3"]@).
prettyMultiScanner :: [String] -> MultiScanner nParams -> String
prettyMultiScanner timeDimNames ms =
  prettyMergedAST [] timeDimNames (buildMergedAST timeDimNames ms)

-- | Pretty-print a merged AST.
--
-- @paramNames@ and @dimNames@ provide human-readable names for
-- parameters and time dimensions.
prettyMergedAST :: [String] -> [String] -> MergedAST -> String
prettyMergedAST paramNames dimNames = go 0
  where
    go depth (MLoop name loBounds hiBounds body) =
      let indent = replicate (2 * depth) ' '
          lo = prettyBounds paramNames dimNames loBounds
          hi = prettyBounds paramNames dimNames hiBounds
      in indent ++ "for " ++ name ++ " = " ++ lo ++ " to " ++ hi ++ ":\n"
         ++ concatMap (go (depth + 1)) body

    go depth (MLet name bound body) =
      let indent = replicate (2 * depth) ' '
      in indent ++ "let " ++ name ++ " = " ++ prettyBound paramNames dimNames bound ++ ":\n"
         ++ concatMap (go (depth + 1)) body

    go depth (MStmtBody name args) =
      let indent = replicate (2 * depth) ' '
      in indent ++ name ++ "(" ++ intercalate ", " args ++ ")\n"

    go depth (MSequence blocks) =
      concatMap (go depth) blocks

prettyBounds :: [String] -> [String] -> [AffineBound] -> String
prettyBounds paramNames dimNames bounds =
  case map (prettyBound paramNames dimNames) bounds of
    [b] -> b
    bs  -> "max(" ++ intercalate ", " bs ++ ")"
