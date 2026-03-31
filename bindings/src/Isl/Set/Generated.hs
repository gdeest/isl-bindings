{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Isl.Set.Generated where

import Isl.Types
import Isl.Monad
import Control.Monad.IO.Class (MonadIO)

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)

foreign import ccall "isl_set_dim" c_dim :: SetRef -> DimType -> IO C.CInt

dim :: SetRef -> DimType -> Int
dim set typ =
    unsafePerformIO $ fromIntegral <$> c_dim set typ


foreign import ccall "isl_set_find_dim_by_id" c_findDimById :: SetRef -> DimType -> IdRef -> IO C.CInt

findDimById :: SetRef -> DimType -> IdRef -> Int
findDimById set typ id =
    unsafePerformIO $ fromIntegral <$> c_findDimById set typ id


foreign import ccall "isl_set_find_dim_by_name" c_findDimByName :: SetRef -> DimType -> C.CString -> IO C.CInt

findDimByName :: SetRef -> DimType -> String -> Int
findDimByName set typ name =
    unsafePerformIO $ do
      name_c <- C.newCString name
      fromIntegral <$> c_findDimByName set typ name_c


foreign import ccall "isl_set_follows_at" c_followsAt :: SetRef -> SetRef -> C.CInt -> IO C.CInt

followsAt :: SetRef -> SetRef -> Int -> Int
followsAt set1 set2 pos =
    unsafePerformIO $ fromIntegral <$> c_followsAt set1 set2 (fromIntegral pos)


foreign import ccall "isl_set_involves_dims" c_involvesDims :: SetRef -> DimType -> C.CUInt -> C.CUInt -> IO C.CInt

involvesDims :: SetRef -> DimType -> Int -> Int -> Int
involvesDims set typ first n =
    unsafePerformIO $ fromIntegral <$> c_involvesDims set typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_set_n_dim" c_nDim :: SetRef -> IO C.CInt

nDim :: SetRef -> Int
nDim set =
    unsafePerformIO $ fromIntegral <$> c_nDim set


foreign import ccall "isl_set_n_param" c_nParam :: SetRef -> IO C.CInt

nParam :: SetRef -> Int
nParam set =
    unsafePerformIO $ fromIntegral <$> c_nParam set


foreign import ccall "isl_set_plain_cmp" c_plainCmp :: SetRef -> SetRef -> IO C.CInt

plainCmp :: SetRef -> SetRef -> Int
plainCmp set1 set2 =
    unsafePerformIO $ fromIntegral <$> c_plainCmp set1 set2


foreign import ccall "isl_set_size" c_size :: SetRef -> IO C.CInt

size :: SetRef -> Int
size set =
    unsafePerformIO $ fromIntegral <$> c_size set


foreign import ccall "isl_set_dump" c_dump :: SetRef -> IO ()

dump :: SetRef -> ()
dump set =
    unsafePerformIO $ c_dump set


foreign import ccall "isl_set_get_dim_name" c_getDimName :: SetRef -> DimType -> C.CUInt -> IO C.CString

getDimName :: SetRef -> DimType -> Int -> String
getDimName set typ pos =
    unsafePerformIO $ C.peekCString =<< c_getDimName set typ (fromIntegral pos)


foreign import ccall "isl_set_get_tuple_name" c_getTupleName :: SetRef -> IO C.CString

getTupleName :: SetRef -> String
getTupleName set =
    unsafePerformIO $ C.peekCString =<< c_getTupleName set


foreign import ccall "isl_set_dim_has_any_lower_bound" c_dimHasAnyLowerBound :: SetRef -> DimType -> C.CUInt -> IO C.CBool

dimHasAnyLowerBound :: SetRef -> DimType -> Int -> Bool
dimHasAnyLowerBound set typ pos =
    unsafePerformIO $ M.toBool <$> c_dimHasAnyLowerBound set typ (fromIntegral pos)


foreign import ccall "isl_set_dim_has_any_upper_bound" c_dimHasAnyUpperBound :: SetRef -> DimType -> C.CUInt -> IO C.CBool

dimHasAnyUpperBound :: SetRef -> DimType -> Int -> Bool
dimHasAnyUpperBound set typ pos =
    unsafePerformIO $ M.toBool <$> c_dimHasAnyUpperBound set typ (fromIntegral pos)


foreign import ccall "isl_set_dim_has_lower_bound" c_dimHasLowerBound :: SetRef -> DimType -> C.CUInt -> IO C.CBool

dimHasLowerBound :: SetRef -> DimType -> Int -> Bool
dimHasLowerBound set typ pos =
    unsafePerformIO $ M.toBool <$> c_dimHasLowerBound set typ (fromIntegral pos)


foreign import ccall "isl_set_dim_has_upper_bound" c_dimHasUpperBound :: SetRef -> DimType -> C.CUInt -> IO C.CBool

dimHasUpperBound :: SetRef -> DimType -> Int -> Bool
dimHasUpperBound set typ pos =
    unsafePerformIO $ M.toBool <$> c_dimHasUpperBound set typ (fromIntegral pos)


foreign import ccall "isl_set_dim_is_bounded" c_dimIsBounded :: SetRef -> DimType -> C.CUInt -> IO C.CBool

dimIsBounded :: SetRef -> DimType -> Int -> Bool
dimIsBounded set typ pos =
    unsafePerformIO $ M.toBool <$> c_dimIsBounded set typ (fromIntegral pos)


foreign import ccall "isl_set_has_dim_id" c_hasDimId :: SetRef -> DimType -> C.CUInt -> IO C.CBool

hasDimId :: SetRef -> DimType -> Int -> Bool
hasDimId set typ pos =
    unsafePerformIO $ M.toBool <$> c_hasDimId set typ (fromIntegral pos)


foreign import ccall "isl_set_has_dim_name" c_hasDimName :: SetRef -> DimType -> C.CUInt -> IO C.CBool

hasDimName :: SetRef -> DimType -> Int -> Bool
hasDimName set typ pos =
    unsafePerformIO $ M.toBool <$> c_hasDimName set typ (fromIntegral pos)


foreign import ccall "isl_set_has_equal_space" c_hasEqualSpace :: SetRef -> SetRef -> IO C.CBool

hasEqualSpace :: SetRef -> SetRef -> Bool
hasEqualSpace set1 set2 =
    unsafePerformIO $ M.toBool <$> c_hasEqualSpace set1 set2


foreign import ccall "isl_set_has_tuple_id" c_hasTupleId :: SetRef -> IO C.CBool

hasTupleId :: SetRef -> Bool
hasTupleId set =
    unsafePerformIO $ M.toBool <$> c_hasTupleId set


foreign import ccall "isl_set_has_tuple_name" c_hasTupleName :: SetRef -> IO C.CBool

hasTupleName :: SetRef -> Bool
hasTupleName set =
    unsafePerformIO $ M.toBool <$> c_hasTupleName set


foreign import ccall "isl_set_is_bounded" c_isBounded :: SetRef -> IO C.CBool

isBounded :: SetRef -> Bool
isBounded set =
    unsafePerformIO $ M.toBool <$> c_isBounded set


foreign import ccall "isl_set_is_box" c_isBox :: SetRef -> IO C.CBool

isBox :: SetRef -> Bool
isBox set =
    unsafePerformIO $ M.toBool <$> c_isBox set


foreign import ccall "isl_set_is_params" c_isParams :: SetRef -> IO C.CBool

isParams :: SetRef -> Bool
isParams set =
    unsafePerformIO $ M.toBool <$> c_isParams set


foreign import ccall "isl_set_plain_is_disjoint" c_plainIsDisjoint :: SetRef -> SetRef -> IO C.CBool

plainIsDisjoint :: SetRef -> SetRef -> Bool
plainIsDisjoint set1 set2 =
    unsafePerformIO $ M.toBool <$> c_plainIsDisjoint set1 set2


foreign import ccall "isl_set_plain_is_empty" c_plainIsEmpty :: SetRef -> IO C.CBool

plainIsEmpty :: SetRef -> Bool
plainIsEmpty set =
    unsafePerformIO $ M.toBool <$> c_plainIsEmpty set


foreign import ccall "isl_set_plain_is_equal" c_plainIsEqual :: SetRef -> SetRef -> IO C.CBool

plainIsEqual :: SetRef -> SetRef -> Bool
plainIsEqual set1 set2 =
    unsafePerformIO $ M.toBool <$> c_plainIsEqual set1 set2


foreign import ccall "isl_set_plain_is_universe" c_plainIsUniverse :: SetRef -> IO C.CBool

plainIsUniverse :: SetRef -> Bool
plainIsUniverse set =
    unsafePerformIO $ M.toBool <$> c_plainIsUniverse set


foreign import ccall "isl_set_add_constraint" c_addConstraint :: Set -> Constraint -> IO Set

addConstraint :: forall m. MonadIO m => Set %1 -> Constraint %1 -> IslT m Set
addConstraint = unsafeCoerce go where
  go :: Set -> Constraint -> IslT m Set
  go set constraint =
    unsafeIslFromIO $ \_ -> c_addConstraint set constraint


foreign import ccall "isl_set_add_dims" c_addDims :: Set -> DimType -> C.CUInt -> IO Set

addDims :: forall m. MonadIO m => Set %1 -> DimType -> Int -> IslT m Set
addDims = unsafeCoerce go where
  go :: Set -> DimType -> Int -> IslT m Set
  go set typ n =
    unsafeIslFromIO $ \_ -> c_addDims set typ (fromIntegral n)


foreign import ccall "isl_set_align_params" c_alignParams :: Set -> Space -> IO Set

alignParams :: forall m. MonadIO m => Set %1 -> Space %1 -> IslT m Set
alignParams = unsafeCoerce go where
  go :: Set -> Space -> IslT m Set
  go set model =
    unsafeIslFromIO $ \_ -> c_alignParams set model


foreign import ccall "isl_set_compute_divs" c_computeDivs :: Set -> IO Set

computeDivs :: forall m. MonadIO m => Set %1 -> IslT m Set
computeDivs = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_computeDivs set


foreign import ccall "isl_set_drop_constraints_involving_dims" c_dropConstraintsInvolvingDims :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Set

dropConstraintsInvolvingDims :: forall m. MonadIO m => Set %1 -> DimType -> Int -> Int -> IslT m Set
dropConstraintsInvolvingDims = unsafeCoerce go where
  go :: Set -> DimType -> Int -> Int -> IslT m Set
  go set typ first n =
    unsafeIslFromIO $ \_ -> c_dropConstraintsInvolvingDims set typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_set_drop_constraints_not_involving_dims" c_dropConstraintsNotInvolvingDims :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Set

dropConstraintsNotInvolvingDims :: forall m. MonadIO m => Set %1 -> DimType -> Int -> Int -> IslT m Set
dropConstraintsNotInvolvingDims = unsafeCoerce go where
  go :: Set -> DimType -> Int -> Int -> IslT m Set
  go set typ first n =
    unsafeIslFromIO $ \_ -> c_dropConstraintsNotInvolvingDims set typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_set_eliminate" c_eliminate :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Set

eliminate :: forall m. MonadIO m => Set %1 -> DimType -> Int -> Int -> IslT m Set
eliminate = unsafeCoerce go where
  go :: Set -> DimType -> Int -> Int -> IslT m Set
  go set typ first n =
    unsafeIslFromIO $ \_ -> c_eliminate set typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_set_eliminate_dims" c_eliminateDims :: Set -> C.CUInt -> C.CUInt -> IO Set

eliminateDims :: forall m. MonadIO m => Set %1 -> Int -> Int -> IslT m Set
eliminateDims = unsafeCoerce go where
  go :: Set -> Int -> Int -> IslT m Set
  go set first n =
    unsafeIslFromIO $ \_ -> c_eliminateDims set (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_set_equate" c_equate :: Set -> DimType -> C.CInt -> DimType -> C.CInt -> IO Set

equate :: forall m. MonadIO m => Set %1 -> DimType -> Int -> DimType -> Int -> IslT m Set
equate = unsafeCoerce go where
  go :: Set -> DimType -> Int -> DimType -> Int -> IslT m Set
  go set type1 pos1 type2 pos2 =
    unsafeIslFromIO $ \_ -> c_equate set type1 (fromIntegral pos1) type2 (fromIntegral pos2)


foreign import ccall "isl_set_fix_dim_si" c_fixDimSi :: Set -> C.CUInt -> C.CInt -> IO Set

fixDimSi :: forall m. MonadIO m => Set %1 -> Int -> Int -> IslT m Set
fixDimSi = unsafeCoerce go where
  go :: Set -> Int -> Int -> IslT m Set
  go set dim value =
    unsafeIslFromIO $ \_ -> c_fixDimSi set (fromIntegral dim) (fromIntegral value)


foreign import ccall "isl_set_fix_si" c_fixSi :: Set -> DimType -> C.CUInt -> C.CInt -> IO Set

fixSi :: forall m. MonadIO m => Set %1 -> DimType -> Int -> Int -> IslT m Set
fixSi = unsafeCoerce go where
  go :: Set -> DimType -> Int -> Int -> IslT m Set
  go set typ pos value =
    unsafeIslFromIO $ \_ -> c_fixSi set typ (fromIntegral pos) (fromIntegral value)


foreign import ccall "isl_set_fix_val" c_fixVal :: Set -> DimType -> C.CUInt -> Val -> IO Set

fixVal :: forall m. MonadIO m => Set %1 -> DimType -> Int -> Val %1 -> IslT m Set
fixVal = unsafeCoerce go where
  go :: Set -> DimType -> Int -> Val -> IslT m Set
  go set typ pos v =
    unsafeIslFromIO $ \_ -> c_fixVal set typ (fromIntegral pos) v


foreign import ccall "isl_set_flat_product" c_flatProduct :: Set -> Set -> IO Set

flatProduct :: forall m. MonadIO m => Set %1 -> Set %1 -> IslT m Set
flatProduct = unsafeCoerce go where
  go :: Set -> Set -> IslT m Set
  go set1 set2 =
    unsafeIslFromIO $ \_ -> c_flatProduct set1 set2


foreign import ccall "isl_set_from_multi_aff" c_fromMultiAff :: MultiAff -> IO Set

fromMultiAff :: forall m. MonadIO m => MultiAff %1 -> IslT m Set
fromMultiAff = unsafeCoerce go where
  go :: MultiAff -> IslT m Set
  go ma =
    unsafeIslFromIO $ \_ -> c_fromMultiAff ma


foreign import ccall "isl_set_from_params" c_fromParams :: Set -> IO Set

fromParams :: forall m. MonadIO m => Set %1 -> IslT m Set
fromParams = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_fromParams set


foreign import ccall "isl_set_from_pw_aff" c_fromPwAff :: PwAff -> IO Set

fromPwAff :: forall m. MonadIO m => PwAff %1 -> IslT m Set
fromPwAff = unsafeCoerce go where
  go :: PwAff -> IslT m Set
  go pwaff =
    unsafeIslFromIO $ \_ -> c_fromPwAff pwaff


foreign import ccall "isl_set_from_pw_multi_aff" c_fromPwMultiAff :: PwMultiAff -> IO Set

fromPwMultiAff :: forall m. MonadIO m => PwMultiAff %1 -> IslT m Set
fromPwMultiAff = unsafeCoerce go where
  go :: PwMultiAff -> IslT m Set
  go pma =
    unsafeIslFromIO $ \_ -> c_fromPwMultiAff pma


foreign import ccall "isl_set_from_union_set" c_fromUnionSet :: UnionSet -> IO Set

fromUnionSet :: forall m. MonadIO m => UnionSet %1 -> IslT m Set
fromUnionSet = unsafeCoerce go where
  go :: UnionSet -> IslT m Set
  go uset =
    unsafeIslFromIO $ \_ -> c_fromUnionSet uset


foreign import ccall "isl_set_gist_basic_set" c_gistBasicSet :: Set -> BasicSet -> IO Set

gistBasicSet :: forall m. MonadIO m => Set %1 -> BasicSet %1 -> IslT m Set
gistBasicSet = unsafeCoerce go where
  go :: Set -> BasicSet -> IslT m Set
  go set context =
    unsafeIslFromIO $ \_ -> c_gistBasicSet set context


foreign import ccall "isl_set_insert_dims" c_insertDims :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Set

insertDims :: forall m. MonadIO m => Set %1 -> DimType -> Int -> Int -> IslT m Set
insertDims = unsafeCoerce go where
  go :: Set -> DimType -> Int -> Int -> IslT m Set
  go set typ pos n =
    unsafeIslFromIO $ \_ -> c_insertDims set typ (fromIntegral pos) (fromIntegral n)


foreign import ccall "isl_set_intersect_factor_domain" c_intersectFactorDomain :: Set -> Set -> IO Set

intersectFactorDomain :: forall m. MonadIO m => Set %1 -> Set %1 -> IslT m Set
intersectFactorDomain = unsafeCoerce go where
  go :: Set -> Set -> IslT m Set
  go set domain =
    unsafeIslFromIO $ \_ -> c_intersectFactorDomain set domain


foreign import ccall "isl_set_intersect_factor_range" c_intersectFactorRange :: Set -> Set -> IO Set

intersectFactorRange :: forall m. MonadIO m => Set %1 -> Set %1 -> IslT m Set
intersectFactorRange = unsafeCoerce go where
  go :: Set -> Set -> IslT m Set
  go set range =
    unsafeIslFromIO $ \_ -> c_intersectFactorRange set range


foreign import ccall "isl_set_lift" c_lift :: Set -> IO Set

lift :: forall m. MonadIO m => Set %1 -> IslT m Set
lift = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_lift set


foreign import ccall "isl_set_lower_bound_si" c_lowerBoundSi :: Set -> DimType -> C.CUInt -> C.CInt -> IO Set

lowerBoundSi :: forall m. MonadIO m => Set %1 -> DimType -> Int -> Int -> IslT m Set
lowerBoundSi = unsafeCoerce go where
  go :: Set -> DimType -> Int -> Int -> IslT m Set
  go set typ pos value =
    unsafeIslFromIO $ \_ -> c_lowerBoundSi set typ (fromIntegral pos) (fromIntegral value)


foreign import ccall "isl_set_lower_bound_val" c_lowerBoundVal :: Set -> DimType -> C.CUInt -> Val -> IO Set

lowerBoundVal :: forall m. MonadIO m => Set %1 -> DimType -> Int -> Val %1 -> IslT m Set
lowerBoundVal = unsafeCoerce go where
  go :: Set -> DimType -> Int -> Val -> IslT m Set
  go set typ pos value =
    unsafeIslFromIO $ \_ -> c_lowerBoundVal set typ (fromIntegral pos) value


foreign import ccall "isl_set_make_disjoint" c_makeDisjoint :: Set -> IO Set

makeDisjoint :: forall m. MonadIO m => Set %1 -> IslT m Set
makeDisjoint = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_makeDisjoint set


foreign import ccall "isl_set_move_dims" c_moveDims :: Set -> DimType -> C.CUInt -> DimType -> C.CUInt -> C.CUInt -> IO Set

moveDims :: forall m. MonadIO m => Set %1 -> DimType -> Int -> DimType -> Int -> Int -> IslT m Set
moveDims = unsafeCoerce go where
  go :: Set -> DimType -> Int -> DimType -> Int -> Int -> IslT m Set
  go set dst_type dst_pos src_type src_pos n =
    unsafeIslFromIO $ \_ -> c_moveDims set dst_type (fromIntegral dst_pos) src_type (fromIntegral src_pos) (fromIntegral n)


foreign import ccall "isl_set_nat_universe" c_natUniverse :: Space -> IO Set

natUniverse :: forall m. MonadIO m => Space %1 -> IslT m Set
natUniverse = unsafeCoerce go where
  go :: Space -> IslT m Set
  go space =
    unsafeIslFromIO $ \_ -> c_natUniverse space


foreign import ccall "isl_set_neg" c_neg :: Set -> IO Set

neg :: forall m. MonadIO m => Set %1 -> IslT m Set
neg = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_neg set


foreign import ccall "isl_set_preimage_multi_aff" c_preimageMultiAff :: Set -> MultiAff -> IO Set

preimageMultiAff :: forall m. MonadIO m => Set %1 -> MultiAff %1 -> IslT m Set
preimageMultiAff = unsafeCoerce go where
  go :: Set -> MultiAff -> IslT m Set
  go set ma =
    unsafeIslFromIO $ \_ -> c_preimageMultiAff set ma


foreign import ccall "isl_set_preimage_pw_multi_aff" c_preimagePwMultiAff :: Set -> PwMultiAff -> IO Set

preimagePwMultiAff :: forall m. MonadIO m => Set %1 -> PwMultiAff %1 -> IslT m Set
preimagePwMultiAff = unsafeCoerce go where
  go :: Set -> PwMultiAff -> IslT m Set
  go set pma =
    unsafeIslFromIO $ \_ -> c_preimagePwMultiAff set pma


foreign import ccall "isl_set_project_out" c_projectOut :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Set

projectOut :: forall m. MonadIO m => Set %1 -> DimType -> Int -> Int -> IslT m Set
projectOut = unsafeCoerce go where
  go :: Set -> DimType -> Int -> Int -> IslT m Set
  go set typ first n =
    unsafeIslFromIO $ \_ -> c_projectOut set typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_set_project_out_param_id" c_projectOutParamId :: Set -> Id -> IO Set

projectOutParamId :: forall m. MonadIO m => Set %1 -> Id %1 -> IslT m Set
projectOutParamId = unsafeCoerce go where
  go :: Set -> Id -> IslT m Set
  go set id =
    unsafeIslFromIO $ \_ -> c_projectOutParamId set id


foreign import ccall "isl_set_remove_dims" c_removeDims :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Set

removeDims :: forall m. MonadIO m => Set %1 -> DimType -> Int -> Int -> IslT m Set
removeDims = unsafeCoerce go where
  go :: Set -> DimType -> Int -> Int -> IslT m Set
  go bset typ first n =
    unsafeIslFromIO $ \_ -> c_removeDims bset typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_set_remove_divs" c_removeDivs :: Set -> IO Set

removeDivs :: forall m. MonadIO m => Set %1 -> IslT m Set
removeDivs = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_removeDivs set


foreign import ccall "isl_set_remove_divs_involving_dims" c_removeDivsInvolvingDims :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Set

removeDivsInvolvingDims :: forall m. MonadIO m => Set %1 -> DimType -> Int -> Int -> IslT m Set
removeDivsInvolvingDims = unsafeCoerce go where
  go :: Set -> DimType -> Int -> Int -> IslT m Set
  go set typ first n =
    unsafeIslFromIO $ \_ -> c_removeDivsInvolvingDims set typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_set_remove_redundancies" c_removeRedundancies :: Set -> IO Set

removeRedundancies :: forall m. MonadIO m => Set %1 -> IslT m Set
removeRedundancies = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_removeRedundancies set


foreign import ccall "isl_set_remove_unknown_divs" c_removeUnknownDivs :: Set -> IO Set

removeUnknownDivs :: forall m. MonadIO m => Set %1 -> IslT m Set
removeUnknownDivs = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_removeUnknownDivs set


foreign import ccall "isl_set_reset_space" c_resetSpace :: Set -> Space -> IO Set

resetSpace :: forall m. MonadIO m => Set %1 -> Space %1 -> IslT m Set
resetSpace = unsafeCoerce go where
  go :: Set -> Space -> IslT m Set
  go set space =
    unsafeIslFromIO $ \_ -> c_resetSpace set space


foreign import ccall "isl_set_reset_tuple_id" c_resetTupleId :: Set -> IO Set

resetTupleId :: forall m. MonadIO m => Set %1 -> IslT m Set
resetTupleId = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_resetTupleId set


foreign import ccall "isl_set_reset_user" c_resetUser :: Set -> IO Set

resetUser :: forall m. MonadIO m => Set %1 -> IslT m Set
resetUser = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_resetUser set


foreign import ccall "isl_set_set_dim_id" c_setDimId :: Set -> DimType -> C.CUInt -> Id -> IO Set

setDimId :: forall m. MonadIO m => Set %1 -> DimType -> Int -> Id %1 -> IslT m Set
setDimId = unsafeCoerce go where
  go :: Set -> DimType -> Int -> Id -> IslT m Set
  go set typ pos id =
    unsafeIslFromIO $ \_ -> c_setDimId set typ (fromIntegral pos) id


foreign import ccall "isl_set_set_dim_name" c_setDimName :: Set -> DimType -> C.CUInt -> C.CString -> IO Set

setDimName :: forall m. MonadIO m => Set %1 -> DimType -> Int -> String -> IslT m Set
setDimName = unsafeCoerce go where
  go :: Set -> DimType -> Int -> String -> IslT m Set
  go set typ pos s =
    unsafeIslFromIO $ \_ -> do
      s_c <- C.newCString s
      c_setDimName set typ (fromIntegral pos) s_c


foreign import ccall "isl_set_set_tuple_id" c_setTupleId :: Set -> Id -> IO Set

setTupleId :: forall m. MonadIO m => Set %1 -> Id %1 -> IslT m Set
setTupleId = unsafeCoerce go where
  go :: Set -> Id -> IslT m Set
  go set id =
    unsafeIslFromIO $ \_ -> c_setTupleId set id


foreign import ccall "isl_set_set_tuple_name" c_setTupleName :: Set -> C.CString -> IO Set

setTupleName :: forall m. MonadIO m => Set %1 -> String -> IslT m Set
setTupleName = unsafeCoerce go where
  go :: Set -> String -> IslT m Set
  go set s =
    unsafeIslFromIO $ \_ -> do
      s_c <- C.newCString s
      c_setTupleName set s_c


foreign import ccall "isl_set_split_dims" c_splitDims :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Set

splitDims :: forall m. MonadIO m => Set %1 -> DimType -> Int -> Int -> IslT m Set
splitDims = unsafeCoerce go where
  go :: Set -> DimType -> Int -> Int -> IslT m Set
  go set typ first n =
    unsafeIslFromIO $ \_ -> c_splitDims set typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_set_sum" c_sum :: Set -> Set -> IO Set

sum :: forall m. MonadIO m => Set %1 -> Set %1 -> IslT m Set
sum = unsafeCoerce go where
  go :: Set -> Set -> IslT m Set
  go set1 set2 =
    unsafeIslFromIO $ \_ -> c_sum set1 set2


foreign import ccall "isl_set_union_disjoint" c_unionDisjoint :: Set -> Set -> IO Set

unionDisjoint :: forall m. MonadIO m => Set %1 -> Set %1 -> IslT m Set
unionDisjoint = unsafeCoerce go where
  go :: Set -> Set -> IslT m Set
  go set1 set2 =
    unsafeIslFromIO $ \_ -> c_unionDisjoint set1 set2


foreign import ccall "isl_set_upper_bound_si" c_upperBoundSi :: Set -> DimType -> C.CUInt -> C.CInt -> IO Set

upperBoundSi :: forall m. MonadIO m => Set %1 -> DimType -> Int -> Int -> IslT m Set
upperBoundSi = unsafeCoerce go where
  go :: Set -> DimType -> Int -> Int -> IslT m Set
  go set typ pos value =
    unsafeIslFromIO $ \_ -> c_upperBoundSi set typ (fromIntegral pos) (fromIntegral value)


foreign import ccall "isl_set_upper_bound_val" c_upperBoundVal :: Set -> DimType -> C.CUInt -> Val -> IO Set

upperBoundVal :: forall m. MonadIO m => Set %1 -> DimType -> Int -> Val %1 -> IslT m Set
upperBoundVal = unsafeCoerce go where
  go :: Set -> DimType -> Int -> Val -> IslT m Set
  go set typ pos value =
    unsafeIslFromIO $ \_ -> c_upperBoundVal set typ (fromIntegral pos) value


foreign import ccall "isl_set_flatten_map" c_flattenMap :: Set -> IO Map

flattenMap :: forall m. MonadIO m => Set %1 -> IslT m Map
flattenMap = unsafeCoerce go where
  go :: Set -> IslT m Map
  go set =
    unsafeIslFromIO $ \_ -> c_flattenMap set


foreign import ccall "isl_set_lex_ge_set" c_lexGeSet :: Set -> Set -> IO Map

lexGeSet :: forall m. MonadIO m => Set %1 -> Set %1 -> IslT m Map
lexGeSet = unsafeCoerce go where
  go :: Set -> Set -> IslT m Map
  go set1 set2 =
    unsafeIslFromIO $ \_ -> c_lexGeSet set1 set2


foreign import ccall "isl_set_lex_gt_set" c_lexGtSet :: Set -> Set -> IO Map

lexGtSet :: forall m. MonadIO m => Set %1 -> Set %1 -> IslT m Map
lexGtSet = unsafeCoerce go where
  go :: Set -> Set -> IslT m Map
  go set1 set2 =
    unsafeIslFromIO $ \_ -> c_lexGtSet set1 set2


foreign import ccall "isl_set_lex_le_set" c_lexLeSet :: Set -> Set -> IO Map

lexLeSet :: forall m. MonadIO m => Set %1 -> Set %1 -> IslT m Map
lexLeSet = unsafeCoerce go where
  go :: Set -> Set -> IslT m Map
  go set1 set2 =
    unsafeIslFromIO $ \_ -> c_lexLeSet set1 set2


foreign import ccall "isl_set_lex_lt_set" c_lexLtSet :: Set -> Set -> IO Map

lexLtSet :: forall m. MonadIO m => Set %1 -> Set %1 -> IslT m Map
lexLtSet = unsafeCoerce go where
  go :: Set -> Set -> IslT m Map
  go set1 set2 =
    unsafeIslFromIO $ \_ -> c_lexLtSet set1 set2


foreign import ccall "isl_set_project_onto_map" c_projectOntoMap :: Set -> DimType -> C.CUInt -> C.CUInt -> IO Map

projectOntoMap :: forall m. MonadIO m => Set %1 -> DimType -> Int -> Int -> IslT m Map
projectOntoMap = unsafeCoerce go where
  go :: Set -> DimType -> Int -> Int -> IslT m Map
  go set typ first n =
    unsafeIslFromIO $ \_ -> c_projectOntoMap set typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_set_wrapped_domain_map" c_wrappedDomainMap :: Set -> IO Map

wrappedDomainMap :: forall m. MonadIO m => Set %1 -> IslT m Map
wrappedDomainMap = unsafeCoerce go where
  go :: Set -> IslT m Map
  go set =
    unsafeIslFromIO $ \_ -> c_wrappedDomainMap set


foreign import ccall "isl_set_bounded_simple_hull" c_boundedSimpleHull :: Set -> IO BasicSet

boundedSimpleHull :: forall m. MonadIO m => Set %1 -> IslT m BasicSet
boundedSimpleHull = unsafeCoerce go where
  go :: Set -> IslT m BasicSet
  go set =
    unsafeIslFromIO $ \_ -> c_boundedSimpleHull set


foreign import ccall "isl_set_coefficients" c_coefficients :: Set -> IO BasicSet

coefficients :: forall m. MonadIO m => Set %1 -> IslT m BasicSet
coefficients = unsafeCoerce go where
  go :: Set -> IslT m BasicSet
  go set =
    unsafeIslFromIO $ \_ -> c_coefficients set


foreign import ccall "isl_set_convex_hull" c_convexHull :: Set -> IO BasicSet

convexHull :: forall m. MonadIO m => Set %1 -> IslT m BasicSet
convexHull = unsafeCoerce go where
  go :: Set -> IslT m BasicSet
  go set =
    unsafeIslFromIO $ \_ -> c_convexHull set


foreign import ccall "isl_set_plain_unshifted_simple_hull" c_plainUnshiftedSimpleHull :: Set -> IO BasicSet

plainUnshiftedSimpleHull :: forall m. MonadIO m => Set %1 -> IslT m BasicSet
plainUnshiftedSimpleHull = unsafeCoerce go where
  go :: Set -> IslT m BasicSet
  go set =
    unsafeIslFromIO $ \_ -> c_plainUnshiftedSimpleHull set


foreign import ccall "isl_set_simple_hull" c_simpleHull :: Set -> IO BasicSet

simpleHull :: forall m. MonadIO m => Set %1 -> IslT m BasicSet
simpleHull = unsafeCoerce go where
  go :: Set -> IslT m BasicSet
  go set =
    unsafeIslFromIO $ \_ -> c_simpleHull set


foreign import ccall "isl_set_solutions" c_solutions :: Set -> IO BasicSet

solutions :: forall m. MonadIO m => Set %1 -> IslT m BasicSet
solutions = unsafeCoerce go where
  go :: Set -> IslT m BasicSet
  go set =
    unsafeIslFromIO $ \_ -> c_solutions set


foreign import ccall "isl_set_count_val" c_countVal :: SetRef -> IO Val

countVal :: MonadIO m => SetRef -> IslT m Val
countVal set =
    unsafeIslFromIO $ \_ -> c_countVal set


foreign import ccall "isl_set_plain_get_val_if_fixed" c_plainGetValIfFixed :: SetRef -> DimType -> C.CUInt -> IO Val

plainGetValIfFixed :: MonadIO m => SetRef -> DimType -> Int -> IslT m Val
plainGetValIfFixed set typ pos =
    unsafeIslFromIO $ \_ -> c_plainGetValIfFixed set typ (fromIntegral pos)


foreign import ccall "isl_set_dim_max" c_dimMax :: Set -> C.CInt -> IO PwAff

dimMax :: forall m. MonadIO m => Set %1 -> Int -> IslT m PwAff
dimMax = unsafeCoerce go where
  go :: Set -> Int -> IslT m PwAff
  go set pos =
    unsafeIslFromIO $ \_ -> c_dimMax set (fromIntegral pos)


foreign import ccall "isl_set_dim_min" c_dimMin :: Set -> C.CInt -> IO PwAff

dimMin :: forall m. MonadIO m => Set %1 -> Int -> IslT m PwAff
dimMin = unsafeCoerce go where
  go :: Set -> Int -> IslT m PwAff
  go set pos =
    unsafeIslFromIO $ \_ -> c_dimMin set (fromIntegral pos)


foreign import ccall "isl_set_param_pw_aff_on_domain_id" c_paramPwAffOnDomainId :: Set -> Id -> IO PwAff

paramPwAffOnDomainId :: forall m. MonadIO m => Set %1 -> Id %1 -> IslT m PwAff
paramPwAffOnDomainId = unsafeCoerce go where
  go :: Set -> Id -> IslT m PwAff
  go domain id =
    unsafeIslFromIO $ \_ -> c_paramPwAffOnDomainId domain id


foreign import ccall "isl_set_pw_aff_on_domain_val" c_pwAffOnDomainVal :: Set -> Val -> IO PwAff

pwAffOnDomainVal :: forall m. MonadIO m => Set %1 -> Val %1 -> IslT m PwAff
pwAffOnDomainVal = unsafeCoerce go where
  go :: Set -> Val -> IslT m PwAff
  go domain v =
    unsafeIslFromIO $ \_ -> c_pwAffOnDomainVal domain v


foreign import ccall "isl_set_get_dim_id" c_getDimId :: SetRef -> DimType -> C.CUInt -> IO Id

getDimId :: MonadIO m => SetRef -> DimType -> Int -> IslT m Id
getDimId set typ pos =
    unsafeIslFromIO $ \_ -> c_getDimId set typ (fromIntegral pos)


foreign import ccall "isl_set_get_tuple_id" c_getTupleId :: SetRef -> IO Id

getTupleId :: MonadIO m => SetRef -> IslT m Id
getTupleId set =
    unsafeIslFromIO $ \_ -> c_getTupleId set


foreign import ccall "isl_set_to_str" c_toStr :: SetRef -> IO C.CString

toStr :: SetRef -> String
toStr set =
    unsafePerformIO $ C.peekCString =<< c_toStr set


foreign import ccall "isl_set_involves_locals" c_involvesLocals :: SetRef -> IO C.CInt

involvesLocals :: SetRef -> Int
involvesLocals set =
    unsafePerformIO $ fromIntegral <$> c_involvesLocals set


foreign import ccall "isl_set_n_basic_set" c_nBasicSet :: SetRef -> IO C.CInt

nBasicSet :: SetRef -> Int
nBasicSet set =
    unsafePerformIO $ fromIntegral <$> c_nBasicSet set


foreign import ccall "isl_set_tuple_dim" c_tupleDim :: SetRef -> IO C.CInt

tupleDim :: SetRef -> Int
tupleDim set =
    unsafePerformIO $ fromIntegral <$> c_tupleDim set


foreign import ccall "isl_set_is_disjoint" c_isDisjoint :: SetRef -> SetRef -> IO C.CBool

isDisjoint :: SetRef -> SetRef -> Bool
isDisjoint set1 set2 =
    unsafePerformIO $ M.toBool <$> c_isDisjoint set1 set2


foreign import ccall "isl_set_is_empty" c_isEmpty :: SetRef -> IO C.CBool

isEmpty :: SetRef -> Bool
isEmpty set =
    unsafePerformIO $ M.toBool <$> c_isEmpty set


foreign import ccall "isl_set_is_equal" c_isEqual :: SetRef -> SetRef -> IO C.CBool

isEqual :: SetRef -> SetRef -> Bool
isEqual set1 set2 =
    unsafePerformIO $ M.toBool <$> c_isEqual set1 set2


foreign import ccall "isl_set_is_singleton" c_isSingleton :: SetRef -> IO C.CBool

isSingleton :: SetRef -> Bool
isSingleton set =
    unsafePerformIO $ M.toBool <$> c_isSingleton set


foreign import ccall "isl_set_is_strict_subset" c_isStrictSubset :: SetRef -> SetRef -> IO C.CBool

isStrictSubset :: SetRef -> SetRef -> Bool
isStrictSubset set1 set2 =
    unsafePerformIO $ M.toBool <$> c_isStrictSubset set1 set2


foreign import ccall "isl_set_is_subset" c_isSubset :: SetRef -> SetRef -> IO C.CBool

isSubset :: SetRef -> SetRef -> Bool
isSubset set1 set2 =
    unsafePerformIO $ M.toBool <$> c_isSubset set1 set2


foreign import ccall "isl_set_is_wrapping" c_isWrapping :: SetRef -> IO C.CBool

isWrapping :: SetRef -> Bool
isWrapping set =
    unsafePerformIO $ M.toBool <$> c_isWrapping set


foreign import ccall "isl_set_apply" c_apply :: Set -> Map -> IO Set

apply :: forall m. MonadIO m => Set %1 -> Map %1 -> IslT m Set
apply = unsafeCoerce go where
  go :: Set -> Map -> IslT m Set
  go set map =
    unsafeIslFromIO $ \_ -> c_apply set map


foreign import ccall "isl_set_coalesce" c_coalesce :: Set -> IO Set

coalesce :: forall m. MonadIO m => Set %1 -> IslT m Set
coalesce = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_coalesce set


foreign import ccall "isl_set_complement" c_complement :: Set -> IO Set

complement :: forall m. MonadIO m => Set %1 -> IslT m Set
complement = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_complement set


foreign import ccall "isl_set_detect_equalities" c_detectEqualities :: Set -> IO Set

detectEqualities :: forall m. MonadIO m => Set %1 -> IslT m Set
detectEqualities = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_detectEqualities set


foreign import ccall "isl_set_drop_unused_params" c_dropUnusedParams :: Set -> IO Set

dropUnusedParams :: forall m. MonadIO m => Set %1 -> IslT m Set
dropUnusedParams = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_dropUnusedParams set


foreign import ccall "isl_set_empty" c_empty :: Space -> IO Set

empty :: forall m. MonadIO m => Space %1 -> IslT m Set
empty = unsafeCoerce go where
  go :: Space -> IslT m Set
  go space =
    unsafeIslFromIO $ \_ -> c_empty space


foreign import ccall "isl_set_flatten" c_flatten :: Set -> IO Set

flatten :: forall m. MonadIO m => Set %1 -> IslT m Set
flatten = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_flatten set


foreign import ccall "isl_set_gist" c_gist :: Set -> Set -> IO Set

gist :: forall m. MonadIO m => Set %1 -> Set %1 -> IslT m Set
gist = unsafeCoerce go where
  go :: Set -> Set -> IslT m Set
  go set context =
    unsafeIslFromIO $ \_ -> c_gist set context


foreign import ccall "isl_set_gist_params" c_gistParams :: Set -> Set -> IO Set

gistParams :: forall m. MonadIO m => Set %1 -> Set %1 -> IslT m Set
gistParams = unsafeCoerce go where
  go :: Set -> Set -> IslT m Set
  go set context =
    unsafeIslFromIO $ \_ -> c_gistParams set context


foreign import ccall "isl_set_intersect" c_intersect :: Set -> Set -> IO Set

intersect :: forall m. MonadIO m => Set %1 -> Set %1 -> IslT m Set
intersect = unsafeCoerce go where
  go :: Set -> Set -> IslT m Set
  go set1 set2 =
    unsafeIslFromIO $ \_ -> c_intersect set1 set2


foreign import ccall "isl_set_intersect_params" c_intersectParams :: Set -> Set -> IO Set

intersectParams :: forall m. MonadIO m => Set %1 -> Set %1 -> IslT m Set
intersectParams = unsafeCoerce go where
  go :: Set -> Set -> IslT m Set
  go set params =
    unsafeIslFromIO $ \_ -> c_intersectParams set params


foreign import ccall "isl_set_lexmax" c_lexmax :: Set -> IO Set

lexmax :: forall m. MonadIO m => Set %1 -> IslT m Set
lexmax = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_lexmax set


foreign import ccall "isl_set_lexmin" c_lexmin :: Set -> IO Set

lexmin :: forall m. MonadIO m => Set %1 -> IslT m Set
lexmin = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_lexmin set


foreign import ccall "isl_set_params" c_params :: Set -> IO Set

params :: forall m. MonadIO m => Set %1 -> IslT m Set
params = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_params set


foreign import ccall "isl_set_product" c_product :: Set -> Set -> IO Set

product :: forall m. MonadIO m => Set %1 -> Set %1 -> IslT m Set
product = unsafeCoerce go where
  go :: Set -> Set -> IslT m Set
  go set1 set2 =
    unsafeIslFromIO $ \_ -> c_product set1 set2


foreign import ccall "isl_set_project_out_all_params" c_projectOutAllParams :: Set -> IO Set

projectOutAllParams :: forall m. MonadIO m => Set %1 -> IslT m Set
projectOutAllParams = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_projectOutAllParams set


foreign import ccall "isl_set_subtract" c_subtract :: Set -> Set -> IO Set

subtract :: forall m. MonadIO m => Set %1 -> Set %1 -> IslT m Set
subtract = unsafeCoerce go where
  go :: Set -> Set -> IslT m Set
  go set1 set2 =
    unsafeIslFromIO $ \_ -> c_subtract set1 set2


foreign import ccall "isl_set_union" c_union :: Set -> Set -> IO Set

union :: forall m. MonadIO m => Set %1 -> Set %1 -> IslT m Set
union = unsafeCoerce go where
  go :: Set -> Set -> IslT m Set
  go set1 set2 =
    unsafeIslFromIO $ \_ -> c_union set1 set2


foreign import ccall "isl_set_universe" c_universe :: Space -> IO Set

universe :: forall m. MonadIO m => Space %1 -> IslT m Set
universe = unsafeCoerce go where
  go :: Space -> IslT m Set
  go space =
    unsafeIslFromIO $ \_ -> c_universe space


foreign import ccall "isl_set_wrapped_reverse" c_wrappedReverse :: Set -> IO Set

wrappedReverse :: forall m. MonadIO m => Set %1 -> IslT m Set
wrappedReverse = unsafeCoerce go where
  go :: Set -> IslT m Set
  go set =
    unsafeIslFromIO $ \_ -> c_wrappedReverse set


foreign import ccall "isl_set_get_space" c_getSpace :: SetRef -> IO Space

getSpace :: MonadIO m => SetRef -> IslT m Space
getSpace set =
    unsafeIslFromIO $ \_ -> c_getSpace set


foreign import ccall "isl_set_identity" c_identity :: Set -> IO Map

identity :: forall m. MonadIO m => Set %1 -> IslT m Map
identity = unsafeCoerce go where
  go :: Set -> IslT m Map
  go set =
    unsafeIslFromIO $ \_ -> c_identity set


foreign import ccall "isl_set_insert_domain" c_insertDomain :: Set -> Space -> IO Map

insertDomain :: forall m. MonadIO m => Set %1 -> Space %1 -> IslT m Map
insertDomain = unsafeCoerce go where
  go :: Set -> Space -> IslT m Map
  go set domain =
    unsafeIslFromIO $ \_ -> c_insertDomain set domain


foreign import ccall "isl_set_translation" c_translation :: Set -> IO Map

translation :: forall m. MonadIO m => Set %1 -> IslT m Map
translation = unsafeCoerce go where
  go :: Set -> IslT m Map
  go deltas =
    unsafeIslFromIO $ \_ -> c_translation deltas


foreign import ccall "isl_set_unwrap" c_unwrap :: Set -> IO Map

unwrap :: forall m. MonadIO m => Set %1 -> IslT m Map
unwrap = unsafeCoerce go where
  go :: Set -> IslT m Map
  go set =
    unsafeIslFromIO $ \_ -> c_unwrap set


foreign import ccall "isl_set_affine_hull" c_affineHull :: Set -> IO BasicSet

affineHull :: forall m. MonadIO m => Set %1 -> IslT m BasicSet
affineHull = unsafeCoerce go where
  go :: Set -> IslT m BasicSet
  go set =
    unsafeIslFromIO $ \_ -> c_affineHull set


foreign import ccall "isl_set_polyhedral_hull" c_polyhedralHull :: Set -> IO BasicSet

polyhedralHull :: forall m. MonadIO m => Set %1 -> IslT m BasicSet
polyhedralHull = unsafeCoerce go where
  go :: Set -> IslT m BasicSet
  go set =
    unsafeIslFromIO $ \_ -> c_polyhedralHull set


foreign import ccall "isl_set_sample" c_sample :: Set -> IO BasicSet

sample :: forall m. MonadIO m => Set %1 -> IslT m BasicSet
sample = unsafeCoerce go where
  go :: Set -> IslT m BasicSet
  go set =
    unsafeIslFromIO $ \_ -> c_sample set


foreign import ccall "isl_set_unshifted_simple_hull" c_unshiftedSimpleHull :: Set -> IO BasicSet

unshiftedSimpleHull :: forall m. MonadIO m => Set %1 -> IslT m BasicSet
unshiftedSimpleHull = unsafeCoerce go where
  go :: Set -> IslT m BasicSet
  go set =
    unsafeIslFromIO $ \_ -> c_unshiftedSimpleHull set


foreign import ccall "isl_set_dim_max_val" c_dimMaxVal :: Set -> C.CInt -> IO Val

dimMaxVal :: forall m. MonadIO m => Set %1 -> Int -> IslT m Val
dimMaxVal = unsafeCoerce go where
  go :: Set -> Int -> IslT m Val
  go set pos =
    unsafeIslFromIO $ \_ -> c_dimMaxVal set (fromIntegral pos)


foreign import ccall "isl_set_dim_min_val" c_dimMinVal :: Set -> C.CInt -> IO Val

dimMinVal :: forall m. MonadIO m => Set %1 -> Int -> IslT m Val
dimMinVal = unsafeCoerce go where
  go :: Set -> Int -> IslT m Val
  go set pos =
    unsafeIslFromIO $ \_ -> c_dimMinVal set (fromIntegral pos)


foreign import ccall "isl_set_get_stride" c_getStride :: SetRef -> C.CInt -> IO Val

getStride :: MonadIO m => SetRef -> Int -> IslT m Val
getStride set pos =
    unsafeIslFromIO $ \_ -> c_getStride set (fromIntegral pos)


foreign import ccall "isl_set_max_val" c_maxVal :: SetRef -> AffRef -> IO Val

maxVal :: MonadIO m => SetRef -> AffRef -> IslT m Val
maxVal set obj =
    unsafeIslFromIO $ \_ -> c_maxVal set obj


foreign import ccall "isl_set_min_val" c_minVal :: SetRef -> AffRef -> IO Val

minVal :: MonadIO m => SetRef -> AffRef -> IslT m Val
minVal set obj =
    unsafeIslFromIO $ \_ -> c_minVal set obj


foreign import ccall "isl_set_indicator_function" c_indicatorFunction :: Set -> IO PwAff

indicatorFunction :: forall m. MonadIO m => Set %1 -> IslT m PwAff
indicatorFunction = unsafeCoerce go where
  go :: Set -> IslT m PwAff
  go set =
    unsafeIslFromIO $ \_ -> c_indicatorFunction set


foreign import ccall "isl_set_to_union_set" c_toUnionSet :: Set -> IO UnionSet

toUnionSet :: forall m. MonadIO m => Set %1 -> IslT m UnionSet
toUnionSet = unsafeCoerce go where
  go :: Set -> IslT m UnionSet
  go set =
    unsafeIslFromIO $ \_ -> c_toUnionSet set


foreign import ccall "isl_set_as_pw_multi_aff" c_asPwMultiAff :: Set -> IO PwMultiAff

asPwMultiAff :: forall m. MonadIO m => Set %1 -> IslT m PwMultiAff
asPwMultiAff = unsafeCoerce go where
  go :: Set -> IslT m PwMultiAff
  go set =
    unsafeIslFromIO $ \_ -> c_asPwMultiAff set


foreign import ccall "isl_set_lexmax_pw_multi_aff" c_lexmaxPwMultiAff :: Set -> IO PwMultiAff

lexmaxPwMultiAff :: forall m. MonadIO m => Set %1 -> IslT m PwMultiAff
lexmaxPwMultiAff = unsafeCoerce go where
  go :: Set -> IslT m PwMultiAff
  go set =
    unsafeIslFromIO $ \_ -> c_lexmaxPwMultiAff set


foreign import ccall "isl_set_lexmin_pw_multi_aff" c_lexminPwMultiAff :: Set -> IO PwMultiAff

lexminPwMultiAff :: forall m. MonadIO m => Set %1 -> IslT m PwMultiAff
lexminPwMultiAff = unsafeCoerce go where
  go :: Set -> IslT m PwMultiAff
  go set =
    unsafeIslFromIO $ \_ -> c_lexminPwMultiAff set


foreign import ccall "isl_set_from_basic_set" c_fromBasicSet :: BasicSet -> IO Set

fromBasicSet :: forall m. MonadIO m => BasicSet %1 -> IslT m Set
fromBasicSet = unsafeCoerce go where
  go :: BasicSet -> IslT m Set
  go bset =
    unsafeIslFromIO $ \_ -> c_fromBasicSet bset


foreign import ccall "isl_set_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO Set

readFromStr :: MonadIO m => String -> IslT m Set
readFromStr str =
    unsafeIslFromIO $ \ctx -> do
      str_c <- C.newCString str
      c_readFromStr ctx str_c


foreign import ccall "isl_set_free" c_free :: Set -> IO ()

instance Consumable Set where
  consume = unsafeCoerce $ \x -> unsafePerformIO (c_free x)


foreign import ccall "isl_set_copy" c_copy :: Set -> IO Set

instance Dupable Set where
  dup = unsafeCoerce $ \x -> unsafePerformIO $ do
    copy <- c_copy x
    return (x, copy)


instance Borrow Set SetRef where
  borrow = unsafeCoerce $ \(Set ptr) f -> let !r = f (SetRef ptr) in (r, Set ptr)


