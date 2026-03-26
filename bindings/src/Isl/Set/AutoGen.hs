{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict #-}

module Isl.Set.AutoGen where

import Control.Monad
import Data.Reflection
import Isl.Types

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce

foreign import ccall "isl_set_find_dim_by_id" c_findDimById :: Set -> DimType -> Id -> IO C.CInt


findDimById :: (Given Ctx) => Set -> DimType -> Id -> Int
findDimById = \set' typ' id' -> 
    unsafePerformIO $ (return . fromIntegral) =<< do
      set <- (return) set'
      typ <- (return) typ'
      id <- (return) id'

      let ctx = given :: Ctx
      c_findDimById set typ id


foreign import ccall "isl_set_find_dim_by_name" c_findDimByName :: Set -> DimType -> C.CString -> IO C.CInt


findDimByName :: (Given Ctx) => Set -> DimType -> String -> Int
findDimByName = \set' typ' name' -> 
    unsafePerformIO $ (return . fromIntegral) =<< do
      set <- (return) set'
      typ <- (return) typ'
      name <- (C.newCString) name'

      let ctx = given :: Ctx
      c_findDimByName set typ name


foreign import ccall "isl_set_follows_at" c_followsAt :: Set -> Set -> C.CInt -> IO C.CInt


followsAt :: (Given Ctx) => Set -> Set -> Int -> Int
followsAt = \set1' set2' pos' -> 
    unsafePerformIO $ (return . fromIntegral) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_followsAt set1 set2 pos


foreign import ccall "isl_set_involves_dims" c_involvesDims :: Set -> DimType -> C.CUInt -> C.CUInt -> IO C.CInt


involvesDims :: (Given Ctx) => Set -> DimType -> Int -> Int -> Int
involvesDims = \set' typ' first' n' -> 
    unsafePerformIO $ (return . fromIntegral) =<< do
      set <- (return) set'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_involvesDims set typ first n


foreign import ccall "isl_set_plain_cmp" c_plainCmp :: Set -> Set -> IO C.CInt


plainCmp :: (Given Ctx) => Set -> Set -> Int
plainCmp = \set1' set2' -> 
    unsafePerformIO $ (return . fromIntegral) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_plainCmp set1 set2


foreign import ccall "isl_set_size" c_size :: Set -> IO C.CInt


size :: (Given Ctx) => Set -> Int
size = \set' -> 
    unsafePerformIO $ (return . fromIntegral) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_size set


foreign import ccall "isl_set_get_ctx" c_getCtx :: Set -> IO Ctx


getCtx :: (Given Ctx) => Set -> Ctx
getCtx = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_getCtx set


foreign import ccall "isl_set_dump" c_dump :: Set -> IO ()


dump :: (Given Ctx) => Set -> ()
dump = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_dump set


foreign import ccall "isl_set_get_dim_name" c_getDimName :: Set -> DimType -> C.CUInt -> IO C.CString


getDimName :: (Given Ctx) => Set -> DimType -> Int -> String
getDimName = \set' typ' pos' -> 
    unsafePerformIO $ (C.peekCString) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDimName set typ pos


foreign import ccall "isl_set_get_tuple_name" c_getTupleName :: Set -> IO C.CString


getTupleName :: (Given Ctx) => Set -> String
getTupleName = \set' -> 
    unsafePerformIO $ (C.peekCString) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_getTupleName set


foreign import ccall "isl_set_dim_has_any_lower_bound" c_dimHasAnyLowerBound :: Set -> DimType -> C.CUInt -> IO C.CBool


dimHasAnyLowerBound :: (Given Ctx) => Set -> DimType -> Int -> Bool
dimHasAnyLowerBound = \set' typ' pos' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_dimHasAnyLowerBound set typ pos


foreign import ccall "isl_set_dim_has_any_upper_bound" c_dimHasAnyUpperBound :: Set -> DimType -> C.CUInt -> IO C.CBool


dimHasAnyUpperBound :: (Given Ctx) => Set -> DimType -> Int -> Bool
dimHasAnyUpperBound = \set' typ' pos' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_dimHasAnyUpperBound set typ pos


foreign import ccall "isl_set_dim_has_lower_bound" c_dimHasLowerBound :: Set -> DimType -> C.CUInt -> IO C.CBool


dimHasLowerBound :: (Given Ctx) => Set -> DimType -> Int -> Bool
dimHasLowerBound = \set' typ' pos' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_dimHasLowerBound set typ pos


foreign import ccall "isl_set_dim_has_upper_bound" c_dimHasUpperBound :: Set -> DimType -> C.CUInt -> IO C.CBool


dimHasUpperBound :: (Given Ctx) => Set -> DimType -> Int -> Bool
dimHasUpperBound = \set' typ' pos' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_dimHasUpperBound set typ pos


foreign import ccall "isl_set_dim_is_bounded" c_dimIsBounded :: Set -> DimType -> C.CUInt -> IO C.CBool


dimIsBounded :: (Given Ctx) => Set -> DimType -> Int -> Bool
dimIsBounded = \set' typ' pos' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_dimIsBounded set typ pos


foreign import ccall "isl_set_has_dim_id" c_hasDimId :: Set -> DimType -> C.CUInt -> IO C.CBool


hasDimId :: (Given Ctx) => Set -> DimType -> Int -> Bool
hasDimId = \set' typ' pos' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_hasDimId set typ pos


foreign import ccall "isl_set_has_dim_name" c_hasDimName :: Set -> DimType -> C.CUInt -> IO C.CBool


hasDimName :: (Given Ctx) => Set -> DimType -> Int -> Bool
hasDimName = \set' typ' pos' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_hasDimName set typ pos


foreign import ccall "isl_set_has_equal_space" c_hasEqualSpace :: Set -> Set -> IO C.CBool


hasEqualSpace :: (Given Ctx) => Set -> Set -> Bool
hasEqualSpace = \set1' set2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_hasEqualSpace set1 set2


foreign import ccall "isl_set_has_tuple_id" c_hasTupleId :: Set -> IO C.CBool


hasTupleId :: (Given Ctx) => Set -> Bool
hasTupleId = \set' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_hasTupleId set


foreign import ccall "isl_set_has_tuple_name" c_hasTupleName :: Set -> IO C.CBool


hasTupleName :: (Given Ctx) => Set -> Bool
hasTupleName = \set' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_hasTupleName set


foreign import ccall "isl_set_is_bounded" c_isBounded :: Set -> IO C.CBool


isBounded :: (Given Ctx) => Set -> Bool
isBounded = \set' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_isBounded set


foreign import ccall "isl_set_is_box" c_isBox :: Set -> IO C.CBool


isBox :: (Given Ctx) => Set -> Bool
isBox = \set' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_isBox set


foreign import ccall "isl_set_is_params" c_isParams :: Set -> IO C.CBool


isParams :: (Given Ctx) => Set -> Bool
isParams = \set' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_isParams set


foreign import ccall "isl_set_plain_is_disjoint" c_plainIsDisjoint :: Set -> Set -> IO C.CBool


plainIsDisjoint :: (Given Ctx) => Set -> Set -> Bool
plainIsDisjoint = \set1' set2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_plainIsDisjoint set1 set2


foreign import ccall "isl_set_plain_is_empty" c_plainIsEmpty :: Set -> IO C.CBool


plainIsEmpty :: (Given Ctx) => Set -> Bool
plainIsEmpty = \set' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_plainIsEmpty set


foreign import ccall "isl_set_plain_is_equal" c_plainIsEqual :: Set -> Set -> IO C.CBool


plainIsEqual :: (Given Ctx) => Set -> Set -> Bool
plainIsEqual = \set1' set2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_plainIsEqual set1 set2


foreign import ccall "isl_set_plain_is_universe" c_plainIsUniverse :: Set -> IO C.CBool


plainIsUniverse :: (Given Ctx) => Set -> Bool
plainIsUniverse = \set' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_plainIsUniverse set


foreign import ccall "isl_set_add_constraint" c_addConstraint :: Set -> Constraint -> IO Set


addConstraint :: (Given Ctx) => Set -> Constraint -> Set
addConstraint = \set' constraint' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      constraint <- (return) constraint'

      let ctx = given :: Ctx
      c_addConstraint set constraint


foreign import ccall "isl_set_add_dims" c_addDims :: Set -> DimType -> C.CUInt -> IO Set


addDims :: (Given Ctx) => Set -> DimType -> Int -> Set
addDims = \set' typ' n' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_addDims set typ n


foreign import ccall "isl_set_align_params" c_alignParams :: Set -> Space -> IO Set


alignParams :: (Given Ctx) => Set -> Space -> Set
alignParams = \set' model' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      model <- (return) model'

      let ctx = given :: Ctx
      c_alignParams set model


foreign import ccall "isl_set_compute_divs" c_computeDivs :: Set -> IO Set


computeDivs :: (Given Ctx) => Set -> Set
computeDivs = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_computeDivs set


foreign import ccall "isl_set_copy" c_copy :: Set -> IO Set


copy :: (Given Ctx) => Set -> Set
copy = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_copy set


foreign import ccall "isl_set_drop_constraints_involving_dims" c_dropConstraintsInvolvingDims :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Set


dropConstraintsInvolvingDims :: (Given Ctx) => Set -> DimType -> Int -> Int -> Set
dropConstraintsInvolvingDims = \set' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_dropConstraintsInvolvingDims set typ first n


foreign import ccall "isl_set_drop_constraints_not_involving_dims" c_dropConstraintsNotInvolvingDims :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Set


dropConstraintsNotInvolvingDims :: (Given Ctx) => Set -> DimType -> Int -> Int -> Set
dropConstraintsNotInvolvingDims = \set' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_dropConstraintsNotInvolvingDims set typ first n


foreign import ccall "isl_set_eliminate" c_eliminate :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Set


eliminate :: (Given Ctx) => Set -> DimType -> Int -> Int -> Set
eliminate = \set' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_eliminate set typ first n


foreign import ccall "isl_set_eliminate_dims" c_eliminateDims :: Set -> C.CUInt -> C.CUInt -> IO Set


eliminateDims :: (Given Ctx) => Set -> Int -> Int -> Set
eliminateDims = \set' first' n' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_eliminateDims set first n


foreign import ccall "isl_set_equate" c_equate :: Set -> DimType -> C.CInt -> DimType -> C.CInt -> IO Set


equate :: (Given Ctx) => Set -> DimType -> Int -> DimType -> Int -> Set
equate = \set' type1' pos1' type2' pos2' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      type1 <- (return) type1'
      pos1 <- (return . fromIntegral) pos1'
      type2 <- (return) type2'
      pos2 <- (return . fromIntegral) pos2'

      let ctx = given :: Ctx
      c_equate set type1 pos1 type2 pos2


foreign import ccall "isl_set_fix_dim_si" c_fixDimSi :: Set -> C.CUInt -> C.CInt -> IO Set


fixDimSi :: (Given Ctx) => Set -> Int -> Int -> Set
fixDimSi = \set' dim' value' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      dim <- (return . fromIntegral) dim'
      value <- (return . fromIntegral) value'

      let ctx = given :: Ctx
      c_fixDimSi set dim value


foreign import ccall "isl_set_fix_si" c_fixSi :: Set -> DimType -> C.CUInt -> C.CInt -> IO Set


fixSi :: (Given Ctx) => Set -> DimType -> Int -> Int -> Set
fixSi = \set' typ' pos' value' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      value <- (return . fromIntegral) value'

      let ctx = given :: Ctx
      c_fixSi set typ pos value


foreign import ccall "isl_set_fix_val" c_fixVal :: Set -> DimType -> C.CUInt -> Val -> IO Set


fixVal :: (Given Ctx) => Set -> DimType -> Int -> Val -> Set
fixVal = \set' typ' pos' v' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      v <- (return) v'

      let ctx = given :: Ctx
      c_fixVal set typ pos v


foreign import ccall "isl_set_flat_product" c_flatProduct :: Set -> Set -> IO Set


flatProduct :: (Given Ctx) => Set -> Set -> Set
flatProduct = \set1' set2' -> 
    unsafePerformIO $ (return) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_flatProduct set1 set2


foreign import ccall "isl_set_from_params" c_fromParams :: Set -> IO Set


fromParams :: (Given Ctx) => Set -> Set
fromParams = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_fromParams set


foreign import ccall "isl_set_from_union_set" c_fromUnionSet :: UnionSet -> IO Set


fromUnionSet :: (Given Ctx) => UnionSet -> Set
fromUnionSet = \uset' -> 
    unsafePerformIO $ (return) =<< do
      uset <- (return) uset'

      let ctx = given :: Ctx
      c_fromUnionSet uset


foreign import ccall "isl_set_gist_basic_set" c_gistBasicSet :: Set -> BasicSet -> IO Set


gistBasicSet :: (Given Ctx) => Set -> BasicSet -> Set
gistBasicSet = \set' context' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      context <- (return) context'

      let ctx = given :: Ctx
      c_gistBasicSet set context


foreign import ccall "isl_set_insert_dims" c_insertDims :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Set


insertDims :: (Given Ctx) => Set -> DimType -> Int -> Int -> Set
insertDims = \set' typ' pos' n' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_insertDims set typ pos n


foreign import ccall "isl_set_intersect_factor_domain" c_intersectFactorDomain :: Set -> Set -> IO Set


intersectFactorDomain :: (Given Ctx) => Set -> Set -> Set
intersectFactorDomain = \set' domain' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      domain <- (return) domain'

      let ctx = given :: Ctx
      c_intersectFactorDomain set domain


foreign import ccall "isl_set_intersect_factor_range" c_intersectFactorRange :: Set -> Set -> IO Set


intersectFactorRange :: (Given Ctx) => Set -> Set -> Set
intersectFactorRange = \set' range' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      range <- (return) range'

      let ctx = given :: Ctx
      c_intersectFactorRange set range


foreign import ccall "isl_set_lift" c_lift :: Set -> IO Set


lift :: (Given Ctx) => Set -> Set
lift = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_lift set


foreign import ccall "isl_set_lower_bound_si" c_lowerBoundSi :: Set -> DimType -> C.CUInt -> C.CInt -> IO Set


lowerBoundSi :: (Given Ctx) => Set -> DimType -> Int -> Int -> Set
lowerBoundSi = \set' typ' pos' value' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      value <- (return . fromIntegral) value'

      let ctx = given :: Ctx
      c_lowerBoundSi set typ pos value


foreign import ccall "isl_set_lower_bound_val" c_lowerBoundVal :: Set -> DimType -> C.CUInt -> Val -> IO Set


lowerBoundVal :: (Given Ctx) => Set -> DimType -> Int -> Val -> Set
lowerBoundVal = \set' typ' pos' value' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      value <- (return) value'

      let ctx = given :: Ctx
      c_lowerBoundVal set typ pos value


foreign import ccall "isl_set_make_disjoint" c_makeDisjoint :: Set -> IO Set


makeDisjoint :: (Given Ctx) => Set -> Set
makeDisjoint = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_makeDisjoint set


foreign import ccall "isl_set_move_dims" c_moveDims :: Set -> DimType -> C.CUInt -> DimType -> C.CUInt -> C.CUInt -> IO Set


moveDims :: (Given Ctx) => Set -> DimType -> Int -> DimType -> Int -> Int -> Set
moveDims = \set' dst_type' dst_pos' src_type' src_pos' n' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      dst_type <- (return) dst_type'
      dst_pos <- (return . fromIntegral) dst_pos'
      src_type <- (return) src_type'
      src_pos <- (return . fromIntegral) src_pos'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_moveDims set dst_type dst_pos src_type src_pos n


foreign import ccall "isl_set_nat_universe" c_natUniverse :: Space -> IO Set


natUniverse :: (Given Ctx) => Space -> Set
natUniverse = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_natUniverse space


foreign import ccall "isl_set_neg" c_neg :: Set -> IO Set


neg :: (Given Ctx) => Set -> Set
neg = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_neg set


foreign import ccall "isl_set_project_out" c_projectOut :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Set


projectOut :: (Given Ctx) => Set -> DimType -> Int -> Int -> Set
projectOut = \set' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_projectOut set typ first n


foreign import ccall "isl_set_project_out_param_id" c_projectOutParamId :: Set -> Id -> IO Set


projectOutParamId :: (Given Ctx) => Set -> Id -> Set
projectOutParamId = \set' id' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      id <- (return) id'

      let ctx = given :: Ctx
      c_projectOutParamId set id


foreign import ccall "isl_set_remove_dims" c_removeDims :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Set


removeDims :: (Given Ctx) => Set -> DimType -> Int -> Int -> Set
removeDims = \bset' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_removeDims bset typ first n


foreign import ccall "isl_set_remove_divs" c_removeDivs :: Set -> IO Set


removeDivs :: (Given Ctx) => Set -> Set
removeDivs = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_removeDivs set


foreign import ccall "isl_set_remove_divs_involving_dims" c_removeDivsInvolvingDims :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Set


removeDivsInvolvingDims :: (Given Ctx) => Set -> DimType -> Int -> Int -> Set
removeDivsInvolvingDims = \set' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_removeDivsInvolvingDims set typ first n


foreign import ccall "isl_set_remove_redundancies" c_removeRedundancies :: Set -> IO Set


removeRedundancies :: (Given Ctx) => Set -> Set
removeRedundancies = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_removeRedundancies set


foreign import ccall "isl_set_remove_unknown_divs" c_removeUnknownDivs :: Set -> IO Set


removeUnknownDivs :: (Given Ctx) => Set -> Set
removeUnknownDivs = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_removeUnknownDivs set


foreign import ccall "isl_set_reset_space" c_resetSpace :: Set -> Space -> IO Set


resetSpace :: (Given Ctx) => Set -> Space -> Set
resetSpace = \set' space' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      space <- (return) space'

      let ctx = given :: Ctx
      c_resetSpace set space


foreign import ccall "isl_set_reset_tuple_id" c_resetTupleId :: Set -> IO Set


resetTupleId :: (Given Ctx) => Set -> Set
resetTupleId = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_resetTupleId set


foreign import ccall "isl_set_reset_user" c_resetUser :: Set -> IO Set


resetUser :: (Given Ctx) => Set -> Set
resetUser = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_resetUser set


foreign import ccall "isl_set_set_dim_id" c_setDimId :: Set -> DimType -> C.CUInt -> Id -> IO Set


setDimId :: (Given Ctx) => Set -> DimType -> Int -> Id -> Set
setDimId = \set' typ' pos' id' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      id <- (return) id'

      let ctx = given :: Ctx
      c_setDimId set typ pos id


foreign import ccall "isl_set_set_dim_name" c_setDimName :: Set -> DimType -> C.CUInt -> C.CString -> IO Set


setDimName :: (Given Ctx) => Set -> DimType -> Int -> String -> Set
setDimName = \set' typ' pos' s' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      s <- (C.newCString) s'

      let ctx = given :: Ctx
      c_setDimName set typ pos s


foreign import ccall "isl_set_set_tuple_id" c_setTupleId :: Set -> Id -> IO Set


setTupleId :: (Given Ctx) => Set -> Id -> Set
setTupleId = \set' id' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      id <- (return) id'

      let ctx = given :: Ctx
      c_setTupleId set id


foreign import ccall "isl_set_set_tuple_name" c_setTupleName :: Set -> C.CString -> IO Set


setTupleName :: (Given Ctx) => Set -> String -> Set
setTupleName = \set' s' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      s <- (C.newCString) s'

      let ctx = given :: Ctx
      c_setTupleName set s


foreign import ccall "isl_set_split_dims" c_splitDims :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Set


splitDims :: (Given Ctx) => Set -> DimType -> Int -> Int -> Set
splitDims = \set' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_splitDims set typ first n


foreign import ccall "isl_set_sum" c_sum :: Set -> Set -> IO Set


sum :: (Given Ctx) => Set -> Set -> Set
sum = \set1' set2' -> 
    unsafePerformIO $ (return) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_sum set1 set2


foreign import ccall "isl_set_union_disjoint" c_unionDisjoint :: Set -> Set -> IO Set


unionDisjoint :: (Given Ctx) => Set -> Set -> Set
unionDisjoint = \set1' set2' -> 
    unsafePerformIO $ (return) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_unionDisjoint set1 set2


foreign import ccall "isl_set_upper_bound_si" c_upperBoundSi :: Set -> DimType -> C.CUInt -> C.CInt -> IO Set


upperBoundSi :: (Given Ctx) => Set -> DimType -> Int -> Int -> Set
upperBoundSi = \set' typ' pos' value' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      value <- (return . fromIntegral) value'

      let ctx = given :: Ctx
      c_upperBoundSi set typ pos value


foreign import ccall "isl_set_upper_bound_val" c_upperBoundVal :: Set -> DimType -> C.CUInt -> Val -> IO Set


upperBoundVal :: (Given Ctx) => Set -> DimType -> Int -> Val -> Set
upperBoundVal = \set' typ' pos' value' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      value <- (return) value'

      let ctx = given :: Ctx
      c_upperBoundVal set typ pos value


foreign import ccall "isl_set_flatten_map" c_flattenMap :: Set -> IO Map


flattenMap :: (Given Ctx) => Set -> Map
flattenMap = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_flattenMap set


foreign import ccall "isl_set_lex_ge_set" c_lexGeSet :: Set -> Set -> IO Map


lexGeSet :: (Given Ctx) => Set -> Set -> Map
lexGeSet = \set1' set2' -> 
    unsafePerformIO $ (return) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_lexGeSet set1 set2


foreign import ccall "isl_set_lex_gt_set" c_lexGtSet :: Set -> Set -> IO Map


lexGtSet :: (Given Ctx) => Set -> Set -> Map
lexGtSet = \set1' set2' -> 
    unsafePerformIO $ (return) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_lexGtSet set1 set2


foreign import ccall "isl_set_lex_le_set" c_lexLeSet :: Set -> Set -> IO Map


lexLeSet :: (Given Ctx) => Set -> Set -> Map
lexLeSet = \set1' set2' -> 
    unsafePerformIO $ (return) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_lexLeSet set1 set2


foreign import ccall "isl_set_lex_lt_set" c_lexLtSet :: Set -> Set -> IO Map


lexLtSet :: (Given Ctx) => Set -> Set -> Map
lexLtSet = \set1' set2' -> 
    unsafePerformIO $ (return) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_lexLtSet set1 set2


foreign import ccall "isl_set_project_onto_map" c_projectOntoMap :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Map


projectOntoMap :: (Given Ctx) => Set -> DimType -> Int -> Int -> Map
projectOntoMap = \set' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_projectOntoMap set typ first n


foreign import ccall "isl_set_wrapped_domain_map" c_wrappedDomainMap :: Set -> IO Map


wrappedDomainMap :: (Given Ctx) => Set -> Map
wrappedDomainMap = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_wrappedDomainMap set


foreign import ccall "isl_set_bounded_simple_hull" c_boundedSimpleHull :: Set -> IO BasicSet


boundedSimpleHull :: (Given Ctx) => Set -> BasicSet
boundedSimpleHull = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_boundedSimpleHull set


foreign import ccall "isl_set_coefficients" c_coefficients :: Set -> IO BasicSet


coefficients :: (Given Ctx) => Set -> BasicSet
coefficients = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_coefficients set


foreign import ccall "isl_set_convex_hull" c_convexHull :: Set -> IO BasicSet


convexHull :: (Given Ctx) => Set -> BasicSet
convexHull = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_convexHull set


foreign import ccall "isl_set_plain_unshifted_simple_hull" c_plainUnshiftedSimpleHull :: Set -> IO BasicSet


plainUnshiftedSimpleHull :: (Given Ctx) => Set -> BasicSet
plainUnshiftedSimpleHull = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_plainUnshiftedSimpleHull set


foreign import ccall "isl_set_simple_hull" c_simpleHull :: Set -> IO BasicSet


simpleHull :: (Given Ctx) => Set -> BasicSet
simpleHull = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_simpleHull set


foreign import ccall "isl_set_solutions" c_solutions :: Set -> IO BasicSet


solutions :: (Given Ctx) => Set -> BasicSet
solutions = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_solutions set


foreign import ccall "isl_set_count_val" c_countVal :: Set -> IO Val


countVal :: (Given Ctx) => Set -> Val
countVal = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_countVal set


foreign import ccall "isl_set_plain_get_val_if_fixed" c_plainGetValIfFixed :: Set -> DimType -> C.CUInt -> IO Val


plainGetValIfFixed :: (Given Ctx) => Set -> DimType -> Int -> Val
plainGetValIfFixed = \set' typ' pos' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_plainGetValIfFixed set typ pos


foreign import ccall "isl_set_get_dim_id" c_getDimId :: Set -> DimType -> C.CUInt -> IO Id


getDimId :: (Given Ctx) => Set -> DimType -> Int -> Id
getDimId = \set' typ' pos' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDimId set typ pos


foreign import ccall "isl_set_get_tuple_id" c_getTupleId :: Set -> IO Id


getTupleId :: (Given Ctx) => Set -> Id
getTupleId = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_getTupleId set


foreign import ccall "isl_set_to_str" c_toStr :: Set -> IO C.CString


toStr :: (Given Ctx) => Set -> String
toStr = \set' -> 
    unsafePerformIO $ (C.peekCString) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_toStr set


foreign import ccall "isl_set_involves_locals" c_involvesLocals :: Set -> IO C.CInt


involvesLocals :: (Given Ctx) => Set -> Int
involvesLocals = \set' -> 
    unsafePerformIO $ (return . fromIntegral) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_involvesLocals set


foreign import ccall "isl_set_is_disjoint" c_isDisjoint :: Set -> Set -> IO C.CBool


isDisjoint :: (Given Ctx) => Set -> Set -> Bool
isDisjoint = \set1' set2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_isDisjoint set1 set2


foreign import ccall "isl_set_is_empty" c_isEmpty :: Set -> IO C.CBool


isEmpty :: (Given Ctx) => Set -> Bool
isEmpty = \set' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_isEmpty set


foreign import ccall "isl_set_is_equal" c_isEqual :: Set -> Set -> IO C.CBool


isEqual :: (Given Ctx) => Set -> Set -> Bool
isEqual = \set1' set2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_isEqual set1 set2


foreign import ccall "isl_set_is_singleton" c_isSingleton :: Set -> IO C.CBool


isSingleton :: (Given Ctx) => Set -> Bool
isSingleton = \set' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_isSingleton set


foreign import ccall "isl_set_is_strict_subset" c_isStrictSubset :: Set -> Set -> IO C.CBool


isStrictSubset :: (Given Ctx) => Set -> Set -> Bool
isStrictSubset = \set1' set2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_isStrictSubset set1 set2


foreign import ccall "isl_set_is_subset" c_isSubset :: Set -> Set -> IO C.CBool


isSubset :: (Given Ctx) => Set -> Set -> Bool
isSubset = \set1' set2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_isSubset set1 set2


foreign import ccall "isl_set_is_wrapping" c_isWrapping :: Set -> IO C.CBool


isWrapping :: (Given Ctx) => Set -> Bool
isWrapping = \set' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_isWrapping set


foreign import ccall "isl_set_apply" c_apply :: Set -> Map -> IO Set


apply :: (Given Ctx) => Set -> Map -> Set
apply = \set' map' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      map <- (return) map'

      let ctx = given :: Ctx
      c_apply set map


foreign import ccall "isl_set_coalesce" c_coalesce :: Set -> IO Set


coalesce :: (Given Ctx) => Set -> Set
coalesce = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_coalesce set


foreign import ccall "isl_set_complement" c_complement :: Set -> IO Set


complement :: (Given Ctx) => Set -> Set
complement = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_complement set


foreign import ccall "isl_set_detect_equalities" c_detectEqualities :: Set -> IO Set


detectEqualities :: (Given Ctx) => Set -> Set
detectEqualities = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_detectEqualities set


foreign import ccall "isl_set_drop_unused_params" c_dropUnusedParams :: Set -> IO Set


dropUnusedParams :: (Given Ctx) => Set -> Set
dropUnusedParams = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_dropUnusedParams set


foreign import ccall "isl_set_empty" c_empty :: Space -> IO Set


empty :: (Given Ctx) => Space -> Set
empty = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_empty space


foreign import ccall "isl_set_flatten" c_flatten :: Set -> IO Set


flatten :: (Given Ctx) => Set -> Set
flatten = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_flatten set


foreign import ccall "isl_set_gist" c_gist :: Set -> Set -> IO Set


gist :: (Given Ctx) => Set -> Set -> Set
gist = \set' context' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      context <- (return) context'

      let ctx = given :: Ctx
      c_gist set context


foreign import ccall "isl_set_gist_params" c_gistParams :: Set -> Set -> IO Set


gistParams :: (Given Ctx) => Set -> Set -> Set
gistParams = \set' context' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      context <- (return) context'

      let ctx = given :: Ctx
      c_gistParams set context


foreign import ccall "isl_set_intersect" c_intersect :: Set -> Set -> IO Set


intersect :: (Given Ctx) => Set -> Set -> Set
intersect = \set1' set2' -> 
    unsafePerformIO $ (return) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_intersect set1 set2


foreign import ccall "isl_set_intersect_params" c_intersectParams :: Set -> Set -> IO Set


intersectParams :: (Given Ctx) => Set -> Set -> Set
intersectParams = \set' params' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      params <- (return) params'

      let ctx = given :: Ctx
      c_intersectParams set params


foreign import ccall "isl_set_lexmax" c_lexmax :: Set -> IO Set


lexmax :: (Given Ctx) => Set -> Set
lexmax = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_lexmax set


foreign import ccall "isl_set_lexmin" c_lexmin :: Set -> IO Set


lexmin :: (Given Ctx) => Set -> Set
lexmin = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_lexmin set


foreign import ccall "isl_set_params" c_params :: Set -> IO Set


params :: (Given Ctx) => Set -> Set
params = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_params set


foreign import ccall "isl_set_product" c_product :: Set -> Set -> IO Set


product :: (Given Ctx) => Set -> Set -> Set
product = \set1' set2' -> 
    unsafePerformIO $ (return) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_product set1 set2


foreign import ccall "isl_set_project_out_all_params" c_projectOutAllParams :: Set -> IO Set


projectOutAllParams :: (Given Ctx) => Set -> Set
projectOutAllParams = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_projectOutAllParams set


foreign import ccall "isl_set_subtract" c_subtract :: Set -> Set -> IO Set


subtract :: (Given Ctx) => Set -> Set -> Set
subtract = \set1' set2' -> 
    unsafePerformIO $ (return) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_subtract set1 set2


foreign import ccall "isl_set_union" c_union :: Set -> Set -> IO Set


union :: (Given Ctx) => Set -> Set -> Set
union = \set1' set2' -> 
    unsafePerformIO $ (return) =<< do
      set1 <- (return) set1'
      set2 <- (return) set2'

      let ctx = given :: Ctx
      c_union set1 set2


foreign import ccall "isl_set_universe" c_universe :: Space -> IO Set


universe :: (Given Ctx) => Space -> Set
universe = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_universe space


foreign import ccall "isl_set_wrapped_reverse" c_wrappedReverse :: Set -> IO Set


wrappedReverse :: (Given Ctx) => Set -> Set
wrappedReverse = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_wrappedReverse set


foreign import ccall "isl_set_get_space" c_getSpace :: Set -> IO Space


getSpace :: (Given Ctx) => Set -> Space
getSpace = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_getSpace set


foreign import ccall "isl_set_identity" c_identity :: Set -> IO Map


identity :: (Given Ctx) => Set -> Map
identity = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_identity set


foreign import ccall "isl_set_insert_domain" c_insertDomain :: Set -> Space -> IO Map


insertDomain :: (Given Ctx) => Set -> Space -> Map
insertDomain = \set' domain' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      domain <- (return) domain'

      let ctx = given :: Ctx
      c_insertDomain set domain


foreign import ccall "isl_set_translation" c_translation :: Set -> IO Map


translation :: (Given Ctx) => Set -> Map
translation = \deltas' -> 
    unsafePerformIO $ (return) =<< do
      deltas <- (return) deltas'

      let ctx = given :: Ctx
      c_translation deltas


foreign import ccall "isl_set_unwrap" c_unwrap :: Set -> IO Map


unwrap :: (Given Ctx) => Set -> Map
unwrap = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_unwrap set


foreign import ccall "isl_set_affine_hull" c_affineHull :: Set -> IO BasicSet


affineHull :: (Given Ctx) => Set -> BasicSet
affineHull = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_affineHull set


foreign import ccall "isl_set_polyhedral_hull" c_polyhedralHull :: Set -> IO BasicSet


polyhedralHull :: (Given Ctx) => Set -> BasicSet
polyhedralHull = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_polyhedralHull set


foreign import ccall "isl_set_sample" c_sample :: Set -> IO BasicSet


sample :: (Given Ctx) => Set -> BasicSet
sample = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_sample set


foreign import ccall "isl_set_unshifted_simple_hull" c_unshiftedSimpleHull :: Set -> IO BasicSet


unshiftedSimpleHull :: (Given Ctx) => Set -> BasicSet
unshiftedSimpleHull = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_unshiftedSimpleHull set


foreign import ccall "isl_set_dim_max_val" c_dimMaxVal :: Set -> C.CInt -> IO Val


dimMaxVal :: (Given Ctx) => Set -> Int -> Val
dimMaxVal = \set' pos' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_dimMaxVal set pos


foreign import ccall "isl_set_dim_min_val" c_dimMinVal :: Set -> C.CInt -> IO Val


dimMinVal :: (Given Ctx) => Set -> Int -> Val
dimMinVal = \set' pos' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_dimMinVal set pos


foreign import ccall "isl_set_get_stride" c_getStride :: Set -> C.CInt -> IO Val


getStride :: (Given Ctx) => Set -> Int -> Val
getStride = \set' pos' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getStride set pos


foreign import ccall "isl_set_max_val" c_maxVal :: Set -> Aff -> IO Val


maxVal :: (Given Ctx) => Set -> Aff -> Val
maxVal = \set' obj' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      obj <- (return) obj'

      let ctx = given :: Ctx
      c_maxVal set obj


foreign import ccall "isl_set_min_val" c_minVal :: Set -> Aff -> IO Val


minVal :: (Given Ctx) => Set -> Aff -> Val
minVal = \set' obj' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      obj <- (return) obj'

      let ctx = given :: Ctx
      c_minVal set obj


foreign import ccall "isl_set_to_union_set" c_toUnionSet :: Set -> IO UnionSet


toUnionSet :: (Given Ctx) => Set -> UnionSet
toUnionSet = \set' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'

      let ctx = given :: Ctx
      c_toUnionSet set


foreign import ccall "isl_set_from_basic_set" c_fromBasicSet :: BasicSet -> IO Set


fromBasicSet :: (Given Ctx) => BasicSet -> Set
fromBasicSet = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_fromBasicSet bset


foreign import ccall "isl_set_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO Set


readFromStr :: (Given Ctx) => String -> Set
readFromStr = \str' -> 
    unsafePerformIO $ (return) =<< do
      str <- (C.newCString) str'

      let ctx = given :: Ctx
      c_readFromStr ctx str


