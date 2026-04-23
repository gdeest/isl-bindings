{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Isl.BasicSet.Generated where

import Isl.Types (DimType(..))
import Isl.Types.Raw
import Isl.Types.Internal (Consumable(..), Borrow(..), Dupable(..))
import Isl.Monad.Internal
import Control.Monad.IO.Class (MonadIO)

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)

foreign import ccall "isl_basic_set_compare_at" c_compareAt :: BasicSetRef s_bset1 -> BasicSetRef s_bset2 -> C.CInt -> IO C.CInt

compareAt :: BasicSetRef s_bset1 -> BasicSetRef s_bset2 -> Int -> Int
compareAt bset1 bset2 pos =
    let !r = unsafePerformIO $ fromIntegral <$> c_compareAt bset1 bset2 (fromIntegral pos) in r


foreign import ccall "isl_basic_set_dim" c_dim :: BasicSetRef s_bset -> DimType -> IO C.CInt

dim :: BasicSetRef s_bset -> DimType -> Int
dim bset typ =
    let !r = unsafePerformIO $ fromIntegral <$> c_dim bset typ in r


foreign import ccall "isl_basic_set_involves_dims" c_involvesDims :: BasicSetRef s_bset -> DimType -> C.CUInt -> C.CUInt -> IO C.CInt

involvesDims :: BasicSetRef s_bset -> DimType -> Int -> Int -> Int
involvesDims bset typ first n =
    let !r = unsafePerformIO $ fromIntegral <$> c_involvesDims bset typ (fromIntegral first) (fromIntegral n) in r


foreign import ccall "isl_basic_set_n_constraint" c_nConstraint :: BasicSetRef s_bset -> IO C.CInt

nConstraint :: BasicSetRef s_bset -> Int
nConstraint bset =
    let !r = unsafePerformIO $ fromIntegral <$> c_nConstraint bset in r


foreign import ccall "isl_basic_set_n_dim" c_nDim :: BasicSetRef s_bset -> IO C.CInt

nDim :: BasicSetRef s_bset -> Int
nDim bset =
    let !r = unsafePerformIO $ fromIntegral <$> c_nDim bset in r


foreign import ccall "isl_basic_set_n_param" c_nParam :: BasicSetRef s_bset -> IO C.CInt

nParam :: BasicSetRef s_bset -> Int
nParam bset =
    let !r = unsafePerformIO $ fromIntegral <$> c_nParam bset in r


foreign import ccall "isl_basic_set_dump" c_dump :: BasicSetRef s_bset -> IO ()

dump :: BasicSetRef s_bset -> ()
dump bset =
    let !r = unsafePerformIO $ c_dump bset in r


foreign import ccall "isl_basic_set_get_dim_name" c_getDimName :: BasicSetRef s_bset -> DimType -> C.CUInt -> IO C.CString

getDimName :: BasicSetRef s_bset -> DimType -> Int -> String
getDimName bset typ pos =
    let !r = unsafePerformIO $ C.peekCString =<< c_getDimName bset typ (fromIntegral pos) in r


foreign import ccall "isl_basic_set_get_tuple_name" c_getTupleName :: BasicSetRef s_bset -> IO C.CString

getTupleName :: BasicSetRef s_bset -> String
getTupleName bset =
    let !r = unsafePerformIO $ C.peekCString =<< c_getTupleName bset in r


foreign import ccall "isl_basic_set_is_bounded" c_isBounded :: BasicSetRef s_bset -> IO C.CBool

isBounded :: BasicSetRef s_bset -> Bool
isBounded bset =
    let !r = unsafePerformIO $ M.toBool <$> c_isBounded bset in r


foreign import ccall "isl_basic_set_is_disjoint" c_isDisjoint :: BasicSetRef s_bset1 -> BasicSetRef s_bset2 -> IO C.CBool

isDisjoint :: BasicSetRef s_bset1 -> BasicSetRef s_bset2 -> Bool
isDisjoint bset1 bset2 =
    let !r = unsafePerformIO $ M.toBool <$> c_isDisjoint bset1 bset2 in r


foreign import ccall "isl_basic_set_is_rational" c_isRational :: BasicSetRef s_bset -> IO C.CBool

isRational :: BasicSetRef s_bset -> Bool
isRational bset =
    let !r = unsafePerformIO $ M.toBool <$> c_isRational bset in r


foreign import ccall "isl_basic_set_is_universe" c_isUniverse :: BasicSetRef s_bset -> IO C.CBool

isUniverse :: BasicSetRef s_bset -> Bool
isUniverse bset =
    let !r = unsafePerformIO $ M.toBool <$> c_isUniverse bset in r


foreign import ccall "isl_basic_set_plain_is_empty" c_plainIsEmpty :: BasicSetRef s_bset -> IO C.CBool

plainIsEmpty :: BasicSetRef s_bset -> Bool
plainIsEmpty bset =
    let !r = unsafePerformIO $ M.toBool <$> c_plainIsEmpty bset in r


foreign import ccall "isl_basic_set_plain_is_equal" c_plainIsEqual :: BasicSetRef s_bset1 -> BasicSetRef s_bset2 -> IO C.CBool

plainIsEqual :: BasicSetRef s_bset1 -> BasicSetRef s_bset2 -> Bool
plainIsEqual bset1 bset2 =
    let !r = unsafePerformIO $ M.toBool <$> c_plainIsEqual bset1 bset2 in r


foreign import ccall "isl_basic_set_plain_is_universe" c_plainIsUniverse :: BasicSetRef s_bset -> IO C.CBool

plainIsUniverse :: BasicSetRef s_bset -> Bool
plainIsUniverse bset =
    let !r = unsafePerformIO $ M.toBool <$> c_plainIsUniverse bset in r


foreign import ccall "isl_basic_set_compute_divs" c_computeDivs :: BasicSet -> IO Set

computeDivs :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m Set
computeDivs = unsafeCoerce go where
  go :: BasicSet -> IslT m Set
  go bset =
    unsafeIslFromIO $ \_ -> c_computeDivs bset


foreign import ccall "isl_basic_set_get_space" c_getSpace :: BasicSetRef s_bset -> IO Space

getSpace :: MonadIO m => BasicSetRef s_bset -> IslT m Space
getSpace bset =
    unsafeIslFromIO $ \_ -> c_getSpace bset


foreign import ccall "isl_basic_set_unwrap" c_unwrap :: BasicSet -> IO BasicMap

unwrap :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m BasicMap
unwrap = unsafeCoerce go where
  go :: BasicSet -> IslT m BasicMap
  go bset =
    unsafeIslFromIO $ \_ -> c_unwrap bset


foreign import ccall "isl_basic_set_add_constraint" c_addConstraint :: BasicSet -> Constraint -> IO BasicSet

addConstraint :: forall m s_bset s_constraint. MonadIO m => BasicSet %1 -> Constraint %1 -> IslT m BasicSet
addConstraint = unsafeCoerce go where
  go :: BasicSet -> Constraint -> IslT m BasicSet
  go bset constraint =
    unsafeIslFromIO $ \_ -> c_addConstraint bset constraint


foreign import ccall "isl_basic_set_add_dims" c_addDims :: BasicSet -> DimType -> C.CUInt -> IO BasicSet

addDims :: forall m s_bset. MonadIO m => BasicSet %1 -> DimType -> Int -> IslT m BasicSet
addDims = unsafeCoerce go where
  go :: BasicSet -> DimType -> Int -> IslT m BasicSet
  go bset typ n =
    unsafeIslFromIO $ \_ -> c_addDims bset typ (fromIntegral n)


foreign import ccall "isl_basic_set_align_params" c_alignParams :: BasicSet -> Space -> IO BasicSet

alignParams :: forall m s_bset s_model. MonadIO m => BasicSet %1 -> Space %1 -> IslT m BasicSet
alignParams = unsafeCoerce go where
  go :: BasicSet -> Space -> IslT m BasicSet
  go bset model =
    unsafeIslFromIO $ \_ -> c_alignParams bset model


foreign import ccall "isl_basic_set_coefficients" c_coefficients :: BasicSet -> IO BasicSet

coefficients :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m BasicSet
coefficients = unsafeCoerce go where
  go :: BasicSet -> IslT m BasicSet
  go bset =
    unsafeIslFromIO $ \_ -> c_coefficients bset


foreign import ccall "isl_basic_set_drop_constraints_involving_dims" c_dropConstraintsInvolvingDims :: BasicSet -> DimType -> C.CUInt -> C.CUInt -> IO BasicSet

dropConstraintsInvolvingDims :: forall m s_bset. MonadIO m => BasicSet %1 -> DimType -> Int -> Int -> IslT m BasicSet
dropConstraintsInvolvingDims = unsafeCoerce go where
  go :: BasicSet -> DimType -> Int -> Int -> IslT m BasicSet
  go bset typ first n =
    unsafeIslFromIO $ \_ -> c_dropConstraintsInvolvingDims bset typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_basic_set_drop_constraints_not_involving_dims" c_dropConstraintsNotInvolvingDims :: BasicSet -> DimType -> C.CUInt -> C.CUInt -> IO BasicSet

dropConstraintsNotInvolvingDims :: forall m s_bset. MonadIO m => BasicSet %1 -> DimType -> Int -> Int -> IslT m BasicSet
dropConstraintsNotInvolvingDims = unsafeCoerce go where
  go :: BasicSet -> DimType -> Int -> Int -> IslT m BasicSet
  go bset typ first n =
    unsafeIslFromIO $ \_ -> c_dropConstraintsNotInvolvingDims bset typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_basic_set_drop_unused_params" c_dropUnusedParams :: BasicSet -> IO BasicSet

dropUnusedParams :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m BasicSet
dropUnusedParams = unsafeCoerce go where
  go :: BasicSet -> IslT m BasicSet
  go bset =
    unsafeIslFromIO $ \_ -> c_dropUnusedParams bset


foreign import ccall "isl_basic_set_eliminate" c_eliminate :: BasicSet -> DimType -> C.CUInt -> C.CUInt -> IO BasicSet

eliminate :: forall m s_bset. MonadIO m => BasicSet %1 -> DimType -> Int -> Int -> IslT m BasicSet
eliminate = unsafeCoerce go where
  go :: BasicSet -> DimType -> Int -> Int -> IslT m BasicSet
  go bset typ first n =
    unsafeIslFromIO $ \_ -> c_eliminate bset typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_basic_set_empty" c_empty :: Space -> IO BasicSet

empty :: forall m s_space. MonadIO m => Space %1 -> IslT m BasicSet
empty = unsafeCoerce go where
  go :: Space -> IslT m BasicSet
  go space =
    unsafeIslFromIO $ \_ -> c_empty space


foreign import ccall "isl_basic_set_fix_si" c_fixSi :: BasicSet -> DimType -> C.CUInt -> C.CInt -> IO BasicSet

fixSi :: forall m s_bset. MonadIO m => BasicSet %1 -> DimType -> Int -> Int -> IslT m BasicSet
fixSi = unsafeCoerce go where
  go :: BasicSet -> DimType -> Int -> Int -> IslT m BasicSet
  go bset typ pos value =
    unsafeIslFromIO $ \_ -> c_fixSi bset typ (fromIntegral pos) (fromIntegral value)


foreign import ccall "isl_basic_set_fix_val" c_fixVal :: BasicSet -> DimType -> C.CUInt -> Val -> IO BasicSet

fixVal :: forall m s_bset s_v. MonadIO m => BasicSet %1 -> DimType -> Int -> Val %1 -> IslT m BasicSet
fixVal = unsafeCoerce go where
  go :: BasicSet -> DimType -> Int -> Val -> IslT m BasicSet
  go bset typ pos v =
    unsafeIslFromIO $ \_ -> c_fixVal bset typ (fromIntegral pos) v


foreign import ccall "isl_basic_set_flat_product" c_flatProduct :: BasicSet -> BasicSet -> IO BasicSet

flatProduct :: forall m s_bset1 s_bset2. MonadIO m => BasicSet %1 -> BasicSet %1 -> IslT m BasicSet
flatProduct = unsafeCoerce go where
  go :: BasicSet -> BasicSet -> IslT m BasicSet
  go bset1 bset2 =
    unsafeIslFromIO $ \_ -> c_flatProduct bset1 bset2


foreign import ccall "isl_basic_set_from_constraint" c_fromConstraint :: Constraint -> IO BasicSet

fromConstraint :: forall m s_constraint. MonadIO m => Constraint %1 -> IslT m BasicSet
fromConstraint = unsafeCoerce go where
  go :: Constraint -> IslT m BasicSet
  go constraint =
    unsafeIslFromIO $ \_ -> c_fromConstraint constraint


foreign import ccall "isl_basic_set_from_multi_aff" c_fromMultiAff :: MultiAff -> IO BasicSet

fromMultiAff :: forall m s_ma. MonadIO m => MultiAff %1 -> IslT m BasicSet
fromMultiAff = unsafeCoerce go where
  go :: MultiAff -> IslT m BasicSet
  go ma =
    unsafeIslFromIO $ \_ -> c_fromMultiAff ma


foreign import ccall "isl_basic_set_from_params" c_fromParams :: BasicSet -> IO BasicSet

fromParams :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m BasicSet
fromParams = unsafeCoerce go where
  go :: BasicSet -> IslT m BasicSet
  go bset =
    unsafeIslFromIO $ \_ -> c_fromParams bset


foreign import ccall "isl_basic_set_insert_dims" c_insertDims :: BasicSet -> DimType -> C.CUInt -> C.CUInt -> IO BasicSet

insertDims :: forall m s_bset. MonadIO m => BasicSet %1 -> DimType -> Int -> Int -> IslT m BasicSet
insertDims = unsafeCoerce go where
  go :: BasicSet -> DimType -> Int -> Int -> IslT m BasicSet
  go bset typ pos n =
    unsafeIslFromIO $ \_ -> c_insertDims bset typ (fromIntegral pos) (fromIntegral n)


foreign import ccall "isl_basic_set_lift" c_lift :: BasicSet -> IO BasicSet

lift :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m BasicSet
lift = unsafeCoerce go where
  go :: BasicSet -> IslT m BasicSet
  go bset =
    unsafeIslFromIO $ \_ -> c_lift bset


foreign import ccall "isl_basic_set_lower_bound_val" c_lowerBoundVal :: BasicSet -> DimType -> C.CUInt -> Val -> IO BasicSet

lowerBoundVal :: forall m s_bset s_value. MonadIO m => BasicSet %1 -> DimType -> Int -> Val %1 -> IslT m BasicSet
lowerBoundVal = unsafeCoerce go where
  go :: BasicSet -> DimType -> Int -> Val -> IslT m BasicSet
  go bset typ pos value =
    unsafeIslFromIO $ \_ -> c_lowerBoundVal bset typ (fromIntegral pos) value


foreign import ccall "isl_basic_set_move_dims" c_moveDims :: BasicSet -> DimType -> C.CUInt -> DimType -> C.CUInt -> C.CUInt -> IO BasicSet

moveDims :: forall m s_bset. MonadIO m => BasicSet %1 -> DimType -> Int -> DimType -> Int -> Int -> IslT m BasicSet
moveDims = unsafeCoerce go where
  go :: BasicSet -> DimType -> Int -> DimType -> Int -> Int -> IslT m BasicSet
  go bset dst_type dst_pos src_type src_pos n =
    unsafeIslFromIO $ \_ -> c_moveDims bset dst_type (fromIntegral dst_pos) src_type (fromIntegral src_pos) (fromIntegral n)


foreign import ccall "isl_basic_set_nat_universe" c_natUniverse :: Space -> IO BasicSet

natUniverse :: forall m s_space. MonadIO m => Space %1 -> IslT m BasicSet
natUniverse = unsafeCoerce go where
  go :: Space -> IslT m BasicSet
  go space =
    unsafeIslFromIO $ \_ -> c_natUniverse space


foreign import ccall "isl_basic_set_neg" c_neg :: BasicSet -> IO BasicSet

neg :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m BasicSet
neg = unsafeCoerce go where
  go :: BasicSet -> IslT m BasicSet
  go bset =
    unsafeIslFromIO $ \_ -> c_neg bset


foreign import ccall "isl_basic_set_positive_orthant" c_positiveOrthant :: Space -> IO BasicSet

positiveOrthant :: forall m s_space. MonadIO m => Space %1 -> IslT m BasicSet
positiveOrthant = unsafeCoerce go where
  go :: Space -> IslT m BasicSet
  go space =
    unsafeIslFromIO $ \_ -> c_positiveOrthant space


foreign import ccall "isl_basic_set_preimage_multi_aff" c_preimageMultiAff :: BasicSet -> MultiAff -> IO BasicSet

preimageMultiAff :: forall m s_bset s_ma. MonadIO m => BasicSet %1 -> MultiAff %1 -> IslT m BasicSet
preimageMultiAff = unsafeCoerce go where
  go :: BasicSet -> MultiAff -> IslT m BasicSet
  go bset ma =
    unsafeIslFromIO $ \_ -> c_preimageMultiAff bset ma


foreign import ccall "isl_basic_set_project_out" c_projectOut :: BasicSet -> DimType -> C.CUInt -> C.CUInt -> IO BasicSet

projectOut :: forall m s_bset. MonadIO m => BasicSet %1 -> DimType -> Int -> Int -> IslT m BasicSet
projectOut = unsafeCoerce go where
  go :: BasicSet -> DimType -> Int -> Int -> IslT m BasicSet
  go bset typ first n =
    unsafeIslFromIO $ \_ -> c_projectOut bset typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_basic_set_remove_dims" c_removeDims :: BasicSet -> DimType -> C.CUInt -> C.CUInt -> IO BasicSet

removeDims :: forall m s_bset. MonadIO m => BasicSet %1 -> DimType -> Int -> Int -> IslT m BasicSet
removeDims = unsafeCoerce go where
  go :: BasicSet -> DimType -> Int -> Int -> IslT m BasicSet
  go bset typ first n =
    unsafeIslFromIO $ \_ -> c_removeDims bset typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_basic_set_remove_divs" c_removeDivs :: BasicSet -> IO BasicSet

removeDivs :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m BasicSet
removeDivs = unsafeCoerce go where
  go :: BasicSet -> IslT m BasicSet
  go bset =
    unsafeIslFromIO $ \_ -> c_removeDivs bset


foreign import ccall "isl_basic_set_remove_divs_involving_dims" c_removeDivsInvolvingDims :: BasicSet -> DimType -> C.CUInt -> C.CUInt -> IO BasicSet

removeDivsInvolvingDims :: forall m s_bset. MonadIO m => BasicSet %1 -> DimType -> Int -> Int -> IslT m BasicSet
removeDivsInvolvingDims = unsafeCoerce go where
  go :: BasicSet -> DimType -> Int -> Int -> IslT m BasicSet
  go bset typ first n =
    unsafeIslFromIO $ \_ -> c_removeDivsInvolvingDims bset typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_basic_set_remove_redundancies" c_removeRedundancies :: BasicSet -> IO BasicSet

removeRedundancies :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m BasicSet
removeRedundancies = unsafeCoerce go where
  go :: BasicSet -> IslT m BasicSet
  go bset =
    unsafeIslFromIO $ \_ -> c_removeRedundancies bset


foreign import ccall "isl_basic_set_remove_unknown_divs" c_removeUnknownDivs :: BasicSet -> IO BasicSet

removeUnknownDivs :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m BasicSet
removeUnknownDivs = unsafeCoerce go where
  go :: BasicSet -> IslT m BasicSet
  go bset =
    unsafeIslFromIO $ \_ -> c_removeUnknownDivs bset


foreign import ccall "isl_basic_set_set_dim_name" c_setDimName :: BasicSet -> DimType -> C.CUInt -> C.CString -> IO BasicSet

setDimName :: forall m s_bset. MonadIO m => BasicSet %1 -> DimType -> Int -> String -> IslT m BasicSet
setDimName = unsafeCoerce go where
  go :: BasicSet -> DimType -> Int -> String -> IslT m BasicSet
  go bset typ pos s =
    unsafeIslFromIO $ \_ -> do
      s_c <- C.newCString s
      c_setDimName bset typ (fromIntegral pos) s_c


foreign import ccall "isl_basic_set_set_tuple_id" c_setTupleId :: BasicSet -> Id -> IO BasicSet

setTupleId :: forall m s_bset s_id. MonadIO m => BasicSet %1 -> Id %1 -> IslT m BasicSet
setTupleId = unsafeCoerce go where
  go :: BasicSet -> Id -> IslT m BasicSet
  go bset id =
    unsafeIslFromIO $ \_ -> c_setTupleId bset id


foreign import ccall "isl_basic_set_set_tuple_name" c_setTupleName :: BasicSet -> C.CString -> IO BasicSet

setTupleName :: forall m s_set. MonadIO m => BasicSet %1 -> String -> IslT m BasicSet
setTupleName = unsafeCoerce go where
  go :: BasicSet -> String -> IslT m BasicSet
  go set s =
    unsafeIslFromIO $ \_ -> do
      s_c <- C.newCString s
      c_setTupleName set s_c


foreign import ccall "isl_basic_set_solutions" c_solutions :: BasicSet -> IO BasicSet

solutions :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m BasicSet
solutions = unsafeCoerce go where
  go :: BasicSet -> IslT m BasicSet
  go bset =
    unsafeIslFromIO $ \_ -> c_solutions bset


foreign import ccall "isl_basic_set_universe" c_universe :: Space -> IO BasicSet

universe :: forall m s_space. MonadIO m => Space %1 -> IslT m BasicSet
universe = unsafeCoerce go where
  go :: Space -> IslT m BasicSet
  go space =
    unsafeIslFromIO $ \_ -> c_universe space


foreign import ccall "isl_basic_set_upper_bound_val" c_upperBoundVal :: BasicSet -> DimType -> C.CUInt -> Val -> IO BasicSet

upperBoundVal :: forall m s_bset s_value. MonadIO m => BasicSet %1 -> DimType -> Int -> Val %1 -> IslT m BasicSet
upperBoundVal = unsafeCoerce go where
  go :: BasicSet -> DimType -> Int -> Val -> IslT m BasicSet
  go bset typ pos value =
    unsafeIslFromIO $ \_ -> c_upperBoundVal bset typ (fromIntegral pos) value


foreign import ccall "isl_basic_set_max_val" c_maxVal :: BasicSetRef s_bset -> AffRef s_obj -> IO Val

maxVal :: MonadIO m => BasicSetRef s_bset -> AffRef s_obj -> IslT m Val
maxVal bset obj =
    unsafeIslFromIO $ \_ -> c_maxVal bset obj


foreign import ccall "isl_basic_set_get_div" c_getDiv :: BasicSetRef s_bset -> C.CInt -> IO Aff

getDiv :: MonadIO m => BasicSetRef s_bset -> Int -> IslT m Aff
getDiv bset pos =
    unsafeIslFromIO $ \_ -> c_getDiv bset (fromIntegral pos)


foreign import ccall "isl_basic_set_get_dim_id" c_getDimId :: BasicSetRef s_bset -> DimType -> C.CUInt -> IO Id

getDimId :: MonadIO m => BasicSetRef s_bset -> DimType -> Int -> IslT m Id
getDimId bset typ pos =
    unsafeIslFromIO $ \_ -> c_getDimId bset typ (fromIntegral pos)


foreign import ccall "isl_basic_set_to_str" c_toStr :: BasicSetRef s_bset -> IO C.CString

toStr :: BasicSetRef s_bset -> String
toStr bset =
    let !r = unsafePerformIO $ C.peekCString =<< c_toStr bset in r


foreign import ccall "isl_basic_set_get_local_space" c_getLocalSpace :: BasicSetRef s_bset -> IO LocalSpace

getLocalSpace :: MonadIO m => BasicSetRef s_bset -> IslT m LocalSpace
getLocalSpace bset =
    unsafeIslFromIO $ \_ -> c_getLocalSpace bset


foreign import ccall "isl_basic_set_is_empty" c_isEmpty :: BasicSetRef s_bset -> IO C.CBool

isEmpty :: BasicSetRef s_bset -> Bool
isEmpty bset =
    let !r = unsafePerformIO $ M.toBool <$> c_isEmpty bset in r


foreign import ccall "isl_basic_set_is_equal" c_isEqual :: BasicSetRef s_bset1 -> BasicSetRef s_bset2 -> IO C.CBool

isEqual :: BasicSetRef s_bset1 -> BasicSetRef s_bset2 -> Bool
isEqual bset1 bset2 =
    let !r = unsafePerformIO $ M.toBool <$> c_isEqual bset1 bset2 in r


foreign import ccall "isl_basic_set_is_subset" c_isSubset :: BasicSetRef s_bset1 -> BasicSetRef s_bset2 -> IO C.CBool

isSubset :: BasicSetRef s_bset1 -> BasicSetRef s_bset2 -> Bool
isSubset bset1 bset2 =
    let !r = unsafePerformIO $ M.toBool <$> c_isSubset bset1 bset2 in r


foreign import ccall "isl_basic_set_is_wrapping" c_isWrapping :: BasicSetRef s_bset -> IO C.CBool

isWrapping :: BasicSetRef s_bset -> Bool
isWrapping bset =
    let !r = unsafePerformIO $ M.toBool <$> c_isWrapping bset in r


foreign import ccall "isl_basic_set_lexmax" c_lexmax :: BasicSet -> IO Set

lexmax :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m Set
lexmax = unsafeCoerce go where
  go :: BasicSet -> IslT m Set
  go bset =
    unsafeIslFromIO $ \_ -> c_lexmax bset


foreign import ccall "isl_basic_set_lexmin" c_lexmin :: BasicSet -> IO Set

lexmin :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m Set
lexmin = unsafeCoerce go where
  go :: BasicSet -> IslT m Set
  go bset =
    unsafeIslFromIO $ \_ -> c_lexmin bset


foreign import ccall "isl_basic_set_to_set" c_toSet :: BasicSet -> IO Set

toSet :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m Set
toSet = unsafeCoerce go where
  go :: BasicSet -> IslT m Set
  go bset =
    unsafeIslFromIO $ \_ -> c_toSet bset


foreign import ccall "isl_basic_set_union" c_union :: BasicSet -> BasicSet -> IO Set

union :: forall m s_bset1 s_bset2. MonadIO m => BasicSet %1 -> BasicSet %1 -> IslT m Set
union = unsafeCoerce go where
  go :: BasicSet -> BasicSet -> IslT m Set
  go bset1 bset2 =
    unsafeIslFromIO $ \_ -> c_union bset1 bset2


foreign import ccall "isl_basic_set_affine_hull" c_affineHull :: BasicSet -> IO BasicSet

affineHull :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m BasicSet
affineHull = unsafeCoerce go where
  go :: BasicSet -> IslT m BasicSet
  go bset =
    unsafeIslFromIO $ \_ -> c_affineHull bset


foreign import ccall "isl_basic_set_apply" c_apply :: BasicSet -> BasicMap -> IO BasicSet

apply :: forall m s_bset s_bmap. MonadIO m => BasicSet %1 -> BasicMap %1 -> IslT m BasicSet
apply = unsafeCoerce go where
  go :: BasicSet -> BasicMap -> IslT m BasicSet
  go bset bmap =
    unsafeIslFromIO $ \_ -> c_apply bset bmap


foreign import ccall "isl_basic_set_detect_equalities" c_detectEqualities :: BasicSet -> IO BasicSet

detectEqualities :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m BasicSet
detectEqualities = unsafeCoerce go where
  go :: BasicSet -> IslT m BasicSet
  go bset =
    unsafeIslFromIO $ \_ -> c_detectEqualities bset


foreign import ccall "isl_basic_set_flatten" c_flatten :: BasicSet -> IO BasicSet

flatten :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m BasicSet
flatten = unsafeCoerce go where
  go :: BasicSet -> IslT m BasicSet
  go bset =
    unsafeIslFromIO $ \_ -> c_flatten bset


foreign import ccall "isl_basic_set_gist" c_gist :: BasicSet -> BasicSet -> IO BasicSet

gist :: forall m s_bset s_context. MonadIO m => BasicSet %1 -> BasicSet %1 -> IslT m BasicSet
gist = unsafeCoerce go where
  go :: BasicSet -> BasicSet -> IslT m BasicSet
  go bset context =
    unsafeIslFromIO $ \_ -> c_gist bset context


foreign import ccall "isl_basic_set_intersect" c_intersect :: BasicSet -> BasicSet -> IO BasicSet

intersect :: forall m s_bset1 s_bset2. MonadIO m => BasicSet %1 -> BasicSet %1 -> IslT m BasicSet
intersect = unsafeCoerce go where
  go :: BasicSet -> BasicSet -> IslT m BasicSet
  go bset1 bset2 =
    unsafeIslFromIO $ \_ -> c_intersect bset1 bset2


foreign import ccall "isl_basic_set_intersect_params" c_intersectParams :: BasicSet -> BasicSet -> IO BasicSet

intersectParams :: forall m s_bset1 s_bset2. MonadIO m => BasicSet %1 -> BasicSet %1 -> IslT m BasicSet
intersectParams = unsafeCoerce go where
  go :: BasicSet -> BasicSet -> IslT m BasicSet
  go bset1 bset2 =
    unsafeIslFromIO $ \_ -> c_intersectParams bset1 bset2


foreign import ccall "isl_basic_set_params" c_params :: BasicSet -> IO BasicSet

params :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m BasicSet
params = unsafeCoerce go where
  go :: BasicSet -> IslT m BasicSet
  go bset =
    unsafeIslFromIO $ \_ -> c_params bset


foreign import ccall "isl_basic_set_sample" c_sample :: BasicSet -> IO BasicSet

sample :: forall m s_bset. MonadIO m => BasicSet %1 -> IslT m BasicSet
sample = unsafeCoerce go where
  go :: BasicSet -> IslT m BasicSet
  go bset =
    unsafeIslFromIO $ \_ -> c_sample bset


foreign import ccall "isl_basic_set_dim_max_val" c_dimMaxVal :: BasicSet -> C.CInt -> IO Val

dimMaxVal :: forall m s_bset. MonadIO m => BasicSet %1 -> Int -> IslT m Val
dimMaxVal = unsafeCoerce go where
  go :: BasicSet -> Int -> IslT m Val
  go bset pos =
    unsafeIslFromIO $ \_ -> c_dimMaxVal bset (fromIntegral pos)


foreign import ccall "isl_basic_set_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO BasicSet

readFromStr :: MonadIO m => String -> IslT m BasicSet
readFromStr str =
    unsafeIslFromIO $ \ctx -> do
      str_c <- C.newCString str
      c_readFromStr ctx str_c


foreign import ccall "isl_basic_set_free" c_free :: BasicSet -> IO ()

instance Consumable BasicSet where
  consume = unsafeCoerce c_free


foreign import ccall "isl_basic_set_copy" c_copy :: BasicSet -> IO BasicSet

instance Dupable BasicSet where
  dup = unsafeCoerce $ \x -> do
    copy <- c_copy x
    return (x, copy)


instance Borrow BasicSet BasicSetRef where
  borrow = unsafeCoerce $ \(BasicSet ptr) f -> let !r = f (BasicSetRef ptr) in (r, BasicSet ptr)


