{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Reduction metadata extracted from elaborated Alpha systems.
--
-- Factored out of 'Alpha.Codegen' so 'Alpha.Compile' can validate
-- reduction-dim annotations without an upward import into the codegen
-- layer.  No logic is added here; the contents are a verbatim move
-- from 'Alpha.Codegen'.
module Alpha.Polyhedral.Reduce
  ( ReduceInfo(..)
  , buildReduceMap
  ) where

import qualified Data.Map.Strict as Map

import qualified Isl.Typed.Constraints as C

import qualified Alpha.Core as Core
import qualified Alpha.Core.Named as Named
import Alpha.Codegen.COp (ReduceOp(..))


-- | Reduction info per /statement/.  'riLogicalArray' threads the
-- equation name (not the statement name) so the pragma / init paths
-- aggregate reductions by buffer, not by statement: two Case branches
-- both reducing into @C_buf@ collapse to one @reduction(+:C_buf[:size])@
-- clause and one identity-init loop.
data ReduceInfo = ReduceInfo
  { riOp           :: !ReduceOp
  , riRedDims      :: !Int
  , riBodyConjs    :: ![C.Conjunction C.SetIx]
  , riLogicalArray :: !String
  }

extractReduceInfo
  :: forall sys d a.
     String -> Int
  -> Core.Expr sys d a
  -> Maybe ReduceInfo
extractReduceInfo eqName nDom (Core.Reduce op _ namedBody _ _) =
  let bodyNS = Named.the namedBody
      nRed   = C.nsNDims bodyNS - nDom
  in Just (ReduceInfo op nRed (C.nsConjs bodyNS) eqName)
extractReduceInfo _ _ _ = Nothing

-- | Keys: @eqName@ for a plain reducing equation, @eqName__brI@ for the
-- reducing branches of a 'Core.Case'.  Per-statement shape supports
-- mixed reducing / non-reducing / differently-shaped branches.
buildReduceMap
  :: forall sys a.
     Core.System sys a -> Map.Map String ReduceInfo
buildReduceMap sys = foldr step Map.empty (Core.sysEqs sys)
  where
    step (Core.SomeEquation vdecl body) acc =
      let eqName = Core.vdName vdecl
          nDom   = Core.vdDims vdecl
      in case body of
        Core.Case _ branches -> addBranchReduces eqName nDom 0 branches acc
        _ -> case extractReduceInfo eqName nDom body of
          Just ri -> Map.insert eqName ri acc
          Nothing -> acc

addBranchReduces
  :: forall sys dom bs a.
     String -> Int -> Int
  -> Core.CaseBranches sys dom bs a
  -> Map.Map String ReduceInfo
  -> Map.Map String ReduceInfo
addBranchReduces _      _    _ Core.BNil acc = acc
addBranchReduces eqName nDom i (Core.BCons _ _ body rest) acc =
  let stmtName = eqName ++ "__br" ++ show i
      acc'     = addBranchReduces eqName nDom (i + 1) rest acc
  in case extractReduceInfo eqName nDom body of
    Just ri -> Map.insert stmtName ri acc'
    Nothing -> acc'
