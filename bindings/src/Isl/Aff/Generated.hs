{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Isl.Aff.Generated where

import Isl.Types
import Isl.Types.Internal (Consumable(..), Borrow(..), Dupable(..))
import Isl.Monad.Internal
import Control.Monad.IO.Class (MonadIO)

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)

foreign import ccall "isl_aff_coefficient_sgn" c_coefficientSgn :: AffRef -> DimType -> C.CInt -> IO C.CInt

coefficientSgn :: AffRef -> DimType -> Int -> Int
coefficientSgn aff typ pos =
    let !r = unsafePerformIO $ fromIntegral <$> c_coefficientSgn aff typ (fromIntegral pos) in r


foreign import ccall "isl_aff_dim" c_dim :: AffRef -> DimType -> IO C.CInt

dim :: AffRef -> DimType -> Int
dim aff typ =
    let !r = unsafePerformIO $ fromIntegral <$> c_dim aff typ in r


foreign import ccall "isl_aff_find_dim_by_name" c_findDimByName :: AffRef -> DimType -> C.CString -> IO C.CInt

findDimByName :: AffRef -> DimType -> String -> Int
findDimByName aff typ name =
    let !r = unsafePerformIO $ do
          name_c <- C.newCString name
          fromIntegral <$> c_findDimByName aff typ name_c
    in r


foreign import ccall "isl_aff_involves_dims" c_involvesDims :: AffRef -> DimType -> C.CUInt -> C.CUInt -> IO C.CInt

involvesDims :: AffRef -> DimType -> Int -> Int -> Int
involvesDims aff typ first n =
    let !r = unsafePerformIO $ fromIntegral <$> c_involvesDims aff typ (fromIntegral first) (fromIntegral n) in r


foreign import ccall "isl_aff_involves_locals" c_involvesLocals :: AffRef -> IO C.CInt

involvesLocals :: AffRef -> Int
involvesLocals aff =
    let !r = unsafePerformIO $ fromIntegral <$> c_involvesLocals aff in r


foreign import ccall "isl_aff_dump" c_dump :: AffRef -> IO ()

dump :: AffRef -> ()
dump aff =
    let !r = unsafePerformIO $ c_dump aff in r


foreign import ccall "isl_aff_get_dim_name" c_getDimName :: AffRef -> DimType -> C.CUInt -> IO C.CString

getDimName :: AffRef -> DimType -> Int -> String
getDimName aff typ pos =
    let !r = unsafePerformIO $ C.peekCString =<< c_getDimName aff typ (fromIntegral pos) in r


foreign import ccall "isl_aff_is_nan" c_isNan :: AffRef -> IO C.CBool

isNan :: AffRef -> Bool
isNan aff =
    let !r = unsafePerformIO $ M.toBool <$> c_isNan aff in r


foreign import ccall "isl_aff_plain_is_zero" c_plainIsZero :: AffRef -> IO C.CBool

plainIsZero :: AffRef -> Bool
plainIsZero aff =
    let !r = unsafePerformIO $ M.toBool <$> c_plainIsZero aff in r


foreign import ccall "isl_aff_get_domain_space" c_getDomainSpace :: AffRef -> IO Space

getDomainSpace :: MonadIO m => AffRef -> IslT m Space
getDomainSpace aff =
    unsafeIslFromIO $ \_ -> c_getDomainSpace aff


foreign import ccall "isl_aff_get_space" c_getSpace :: AffRef -> IO Space

getSpace :: MonadIO m => AffRef -> IslT m Space
getSpace aff =
    unsafeIslFromIO $ \_ -> c_getSpace aff


foreign import ccall "isl_aff_bind_id" c_bindId :: Aff -> Id -> IO BasicSet

bindId :: forall m. MonadIO m => Aff %1 -> Id %1 -> IslT m BasicSet
bindId = unsafeCoerce go where
  go :: Aff -> Id -> IslT m BasicSet
  go aff id =
    unsafeIslFromIO $ \_ -> c_bindId aff id


foreign import ccall "isl_aff_eq_basic_set" c_eqBasicSet :: Aff -> Aff -> IO BasicSet

eqBasicSet :: forall m. MonadIO m => Aff %1 -> Aff %1 -> IslT m BasicSet
eqBasicSet = unsafeCoerce go where
  go :: Aff -> Aff -> IslT m BasicSet
  go aff1 aff2 =
    unsafeIslFromIO $ \_ -> c_eqBasicSet aff1 aff2


foreign import ccall "isl_aff_ge_basic_set" c_geBasicSet :: Aff -> Aff -> IO BasicSet

geBasicSet :: forall m. MonadIO m => Aff %1 -> Aff %1 -> IslT m BasicSet
geBasicSet = unsafeCoerce go where
  go :: Aff -> Aff -> IslT m BasicSet
  go aff1 aff2 =
    unsafeIslFromIO $ \_ -> c_geBasicSet aff1 aff2


foreign import ccall "isl_aff_gt_basic_set" c_gtBasicSet :: Aff -> Aff -> IO BasicSet

gtBasicSet :: forall m. MonadIO m => Aff %1 -> Aff %1 -> IslT m BasicSet
gtBasicSet = unsafeCoerce go where
  go :: Aff -> Aff -> IslT m BasicSet
  go aff1 aff2 =
    unsafeIslFromIO $ \_ -> c_gtBasicSet aff1 aff2


foreign import ccall "isl_aff_le_basic_set" c_leBasicSet :: Aff -> Aff -> IO BasicSet

leBasicSet :: forall m. MonadIO m => Aff %1 -> Aff %1 -> IslT m BasicSet
leBasicSet = unsafeCoerce go where
  go :: Aff -> Aff -> IslT m BasicSet
  go aff1 aff2 =
    unsafeIslFromIO $ \_ -> c_leBasicSet aff1 aff2


foreign import ccall "isl_aff_lt_basic_set" c_ltBasicSet :: Aff -> Aff -> IO BasicSet

ltBasicSet :: forall m. MonadIO m => Aff %1 -> Aff %1 -> IslT m BasicSet
ltBasicSet = unsafeCoerce go where
  go :: Aff -> Aff -> IslT m BasicSet
  go aff1 aff2 =
    unsafeIslFromIO $ \_ -> c_ltBasicSet aff1 aff2


foreign import ccall "isl_aff_neg_basic_set" c_negBasicSet :: Aff -> IO BasicSet

negBasicSet :: forall m. MonadIO m => Aff %1 -> IslT m BasicSet
negBasicSet = unsafeCoerce go where
  go :: Aff -> IslT m BasicSet
  go aff =
    unsafeIslFromIO $ \_ -> c_negBasicSet aff


foreign import ccall "isl_aff_zero_basic_set" c_zeroBasicSet :: Aff -> IO BasicSet

zeroBasicSet :: forall m. MonadIO m => Aff %1 -> IslT m BasicSet
zeroBasicSet = unsafeCoerce go where
  go :: Aff -> IslT m BasicSet
  go aff =
    unsafeIslFromIO $ \_ -> c_zeroBasicSet aff


foreign import ccall "isl_aff_get_coefficient_val" c_getCoefficientVal :: AffRef -> DimType -> C.CInt -> IO Val

getCoefficientVal :: MonadIO m => AffRef -> DimType -> Int -> IslT m Val
getCoefficientVal aff typ pos =
    unsafeIslFromIO $ \_ -> c_getCoefficientVal aff typ (fromIntegral pos)


foreign import ccall "isl_aff_get_denominator_val" c_getDenominatorVal :: AffRef -> IO Val

getDenominatorVal :: MonadIO m => AffRef -> IslT m Val
getDenominatorVal aff =
    unsafeIslFromIO $ \_ -> c_getDenominatorVal aff


foreign import ccall "isl_aff_add_coefficient_si" c_addCoefficientSi :: Aff -> DimType -> C.CInt -> C.CInt -> IO Aff

addCoefficientSi :: forall m. MonadIO m => Aff %1 -> DimType -> Int -> Int -> IslT m Aff
addCoefficientSi = unsafeCoerce go where
  go :: Aff -> DimType -> Int -> Int -> IslT m Aff
  go aff typ pos v =
    unsafeIslFromIO $ \_ -> c_addCoefficientSi aff typ (fromIntegral pos) (fromIntegral v)


foreign import ccall "isl_aff_add_coefficient_val" c_addCoefficientVal :: Aff -> DimType -> C.CInt -> Val -> IO Aff

addCoefficientVal :: forall m. MonadIO m => Aff %1 -> DimType -> Int -> Val %1 -> IslT m Aff
addCoefficientVal = unsafeCoerce go where
  go :: Aff -> DimType -> Int -> Val -> IslT m Aff
  go aff typ pos v =
    unsafeIslFromIO $ \_ -> c_addCoefficientVal aff typ (fromIntegral pos) v


foreign import ccall "isl_aff_add_constant_num_si" c_addConstantNumSi :: Aff -> C.CInt -> IO Aff

addConstantNumSi :: forall m. MonadIO m => Aff %1 -> Int -> IslT m Aff
addConstantNumSi = unsafeCoerce go where
  go :: Aff -> Int -> IslT m Aff
  go aff v =
    unsafeIslFromIO $ \_ -> c_addConstantNumSi aff (fromIntegral v)


foreign import ccall "isl_aff_add_constant_si" c_addConstantSi :: Aff -> C.CInt -> IO Aff

addConstantSi :: forall m. MonadIO m => Aff %1 -> Int -> IslT m Aff
addConstantSi = unsafeCoerce go where
  go :: Aff -> Int -> IslT m Aff
  go aff v =
    unsafeIslFromIO $ \_ -> c_addConstantSi aff (fromIntegral v)


foreign import ccall "isl_aff_add_constant_val" c_addConstantVal :: Aff -> Val -> IO Aff

addConstantVal :: forall m. MonadIO m => Aff %1 -> Val %1 -> IslT m Aff
addConstantVal = unsafeCoerce go where
  go :: Aff -> Val -> IslT m Aff
  go aff v =
    unsafeIslFromIO $ \_ -> c_addConstantVal aff v


foreign import ccall "isl_aff_add_dims" c_addDims :: Aff -> DimType -> C.CUInt -> IO Aff

addDims :: forall m. MonadIO m => Aff %1 -> DimType -> Int -> IslT m Aff
addDims = unsafeCoerce go where
  go :: Aff -> DimType -> Int -> IslT m Aff
  go aff typ n =
    unsafeIslFromIO $ \_ -> c_addDims aff typ (fromIntegral n)


foreign import ccall "isl_aff_align_params" c_alignParams :: Aff -> Space -> IO Aff

alignParams :: forall m. MonadIO m => Aff %1 -> Space %1 -> IslT m Aff
alignParams = unsafeCoerce go where
  go :: Aff -> Space -> IslT m Aff
  go aff model =
    unsafeIslFromIO $ \_ -> c_alignParams aff model


foreign import ccall "isl_aff_drop_dims" c_dropDims :: Aff -> DimType -> C.CUInt -> C.CUInt -> IO Aff

dropDims :: forall m. MonadIO m => Aff %1 -> DimType -> Int -> Int -> IslT m Aff
dropDims = unsafeCoerce go where
  go :: Aff -> DimType -> Int -> Int -> IslT m Aff
  go aff typ first n =
    unsafeIslFromIO $ \_ -> c_dropDims aff typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_aff_from_range" c_fromRange :: Aff -> IO Aff

fromRange :: forall m. MonadIO m => Aff %1 -> IslT m Aff
fromRange = unsafeCoerce go where
  go :: Aff -> IslT m Aff
  go aff =
    unsafeIslFromIO $ \_ -> c_fromRange aff


foreign import ccall "isl_aff_get_div" c_getDiv :: AffRef -> C.CInt -> IO Aff

getDiv :: MonadIO m => AffRef -> Int -> IslT m Aff
getDiv aff pos =
    unsafeIslFromIO $ \_ -> c_getDiv aff (fromIntegral pos)


foreign import ccall "isl_aff_insert_dims" c_insertDims :: Aff -> DimType -> C.CUInt -> C.CUInt -> IO Aff

insertDims :: forall m. MonadIO m => Aff %1 -> DimType -> Int -> Int -> IslT m Aff
insertDims = unsafeCoerce go where
  go :: Aff -> DimType -> Int -> Int -> IslT m Aff
  go aff typ first n =
    unsafeIslFromIO $ \_ -> c_insertDims aff typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_aff_mod_val" c_modVal :: Aff -> ValRef -> IO Aff

modVal :: forall m. MonadIO m => Aff %1 -> ValRef -> IslT m Aff
modVal = unsafeCoerce go where
  go :: Aff -> ValRef -> IslT m Aff
  go aff modulo =
    unsafeIslFromIO $ \_ -> c_modVal aff modulo


foreign import ccall "isl_aff_move_dims" c_moveDims :: Aff -> DimType -> C.CUInt -> DimType -> C.CUInt -> C.CUInt -> IO Aff

moveDims :: forall m. MonadIO m => Aff %1 -> DimType -> Int -> DimType -> Int -> Int -> IslT m Aff
moveDims = unsafeCoerce go where
  go :: Aff -> DimType -> Int -> DimType -> Int -> Int -> IslT m Aff
  go aff dst_type dst_pos src_type src_pos n =
    unsafeIslFromIO $ \_ -> c_moveDims aff dst_type (fromIntegral dst_pos) src_type (fromIntegral src_pos) (fromIntegral n)


foreign import ccall "isl_aff_nan_on_domain" c_nanOnDomain :: LocalSpace -> IO Aff

nanOnDomain :: forall m. MonadIO m => LocalSpace %1 -> IslT m Aff
nanOnDomain = unsafeCoerce go where
  go :: LocalSpace -> IslT m Aff
  go ls =
    unsafeIslFromIO $ \_ -> c_nanOnDomain ls


foreign import ccall "isl_aff_nan_on_domain_space" c_nanOnDomainSpace :: Space -> IO Aff

nanOnDomainSpace :: forall m. MonadIO m => Space %1 -> IslT m Aff
nanOnDomainSpace = unsafeCoerce go where
  go :: Space -> IslT m Aff
  go space =
    unsafeIslFromIO $ \_ -> c_nanOnDomainSpace space


foreign import ccall "isl_aff_param_on_domain_space_id" c_paramOnDomainSpaceId :: Space -> Id -> IO Aff

paramOnDomainSpaceId :: forall m. MonadIO m => Space %1 -> Id %1 -> IslT m Aff
paramOnDomainSpaceId = unsafeCoerce go where
  go :: Space -> Id -> IslT m Aff
  go space id =
    unsafeIslFromIO $ \_ -> c_paramOnDomainSpaceId space id


foreign import ccall "isl_aff_project_domain_on_params" c_projectDomainOnParams :: Aff -> IO Aff

projectDomainOnParams :: forall m. MonadIO m => Aff %1 -> IslT m Aff
projectDomainOnParams = unsafeCoerce go where
  go :: Aff -> IslT m Aff
  go aff =
    unsafeIslFromIO $ \_ -> c_projectDomainOnParams aff


foreign import ccall "isl_aff_pullback_aff" c_pullbackAff :: Aff -> Aff -> IO Aff

pullbackAff :: forall m. MonadIO m => Aff %1 -> Aff %1 -> IslT m Aff
pullbackAff = unsafeCoerce go where
  go :: Aff -> Aff -> IslT m Aff
  go aff1 aff2 =
    unsafeIslFromIO $ \_ -> c_pullbackAff aff1 aff2


foreign import ccall "isl_aff_pullback_multi_aff" c_pullbackMultiAff :: Aff -> MultiAff -> IO Aff

pullbackMultiAff :: forall m. MonadIO m => Aff %1 -> MultiAff %1 -> IslT m Aff
pullbackMultiAff = unsafeCoerce go where
  go :: Aff -> MultiAff -> IslT m Aff
  go aff ma =
    unsafeIslFromIO $ \_ -> c_pullbackMultiAff aff ma


foreign import ccall "isl_aff_scale_down_ui" c_scaleDownUi :: Aff -> C.CUInt -> IO Aff

scaleDownUi :: forall m. MonadIO m => Aff %1 -> Int -> IslT m Aff
scaleDownUi = unsafeCoerce go where
  go :: Aff -> Int -> IslT m Aff
  go aff f =
    unsafeIslFromIO $ \_ -> c_scaleDownUi aff (fromIntegral f)


foreign import ccall "isl_aff_scale_down_val" c_scaleDownVal :: Aff -> Val -> IO Aff

scaleDownVal :: forall m. MonadIO m => Aff %1 -> Val %1 -> IslT m Aff
scaleDownVal = unsafeCoerce go where
  go :: Aff -> Val -> IslT m Aff
  go aff v =
    unsafeIslFromIO $ \_ -> c_scaleDownVal aff v


foreign import ccall "isl_aff_scale_val" c_scaleVal :: Aff -> Val -> IO Aff

scaleVal :: forall m. MonadIO m => Aff %1 -> Val %1 -> IslT m Aff
scaleVal = unsafeCoerce go where
  go :: Aff -> Val -> IslT m Aff
  go aff v =
    unsafeIslFromIO $ \_ -> c_scaleVal aff v


foreign import ccall "isl_aff_set_coefficient_si" c_setCoefficientSi :: Aff -> DimType -> C.CInt -> C.CInt -> IO Aff

setCoefficientSi :: forall m. MonadIO m => Aff %1 -> DimType -> Int -> Int -> IslT m Aff
setCoefficientSi = unsafeCoerce go where
  go :: Aff -> DimType -> Int -> Int -> IslT m Aff
  go aff typ pos v =
    unsafeIslFromIO $ \_ -> c_setCoefficientSi aff typ (fromIntegral pos) (fromIntegral v)


foreign import ccall "isl_aff_set_coefficient_val" c_setCoefficientVal :: Aff -> DimType -> C.CInt -> Val -> IO Aff

setCoefficientVal :: forall m. MonadIO m => Aff %1 -> DimType -> Int -> Val %1 -> IslT m Aff
setCoefficientVal = unsafeCoerce go where
  go :: Aff -> DimType -> Int -> Val -> IslT m Aff
  go aff typ pos v =
    unsafeIslFromIO $ \_ -> c_setCoefficientVal aff typ (fromIntegral pos) v


foreign import ccall "isl_aff_set_constant_si" c_setConstantSi :: Aff -> C.CInt -> IO Aff

setConstantSi :: forall m. MonadIO m => Aff %1 -> Int -> IslT m Aff
setConstantSi = unsafeCoerce go where
  go :: Aff -> Int -> IslT m Aff
  go aff v =
    unsafeIslFromIO $ \_ -> c_setConstantSi aff (fromIntegral v)


foreign import ccall "isl_aff_set_constant_val" c_setConstantVal :: Aff -> Val -> IO Aff

setConstantVal :: forall m. MonadIO m => Aff %1 -> Val %1 -> IslT m Aff
setConstantVal = unsafeCoerce go where
  go :: Aff -> Val -> IslT m Aff
  go aff v =
    unsafeIslFromIO $ \_ -> c_setConstantVal aff v


foreign import ccall "isl_aff_set_dim_id" c_setDimId :: Aff -> DimType -> C.CUInt -> Id -> IO Aff

setDimId :: forall m. MonadIO m => Aff %1 -> DimType -> Int -> Id %1 -> IslT m Aff
setDimId = unsafeCoerce go where
  go :: Aff -> DimType -> Int -> Id -> IslT m Aff
  go aff typ pos id =
    unsafeIslFromIO $ \_ -> c_setDimId aff typ (fromIntegral pos) id


foreign import ccall "isl_aff_set_dim_name" c_setDimName :: Aff -> DimType -> C.CUInt -> C.CString -> IO Aff

setDimName :: forall m. MonadIO m => Aff %1 -> DimType -> Int -> String -> IslT m Aff
setDimName = unsafeCoerce go where
  go :: Aff -> DimType -> Int -> String -> IslT m Aff
  go aff typ pos s =
    unsafeIslFromIO $ \_ -> do
      s_c <- C.newCString s
      c_setDimName aff typ (fromIntegral pos) s_c


foreign import ccall "isl_aff_set_tuple_id" c_setTupleId :: Aff -> DimType -> Id -> IO Aff

setTupleId :: forall m. MonadIO m => Aff %1 -> DimType -> Id %1 -> IslT m Aff
setTupleId = unsafeCoerce go where
  go :: Aff -> DimType -> Id -> IslT m Aff
  go aff typ id =
    unsafeIslFromIO $ \_ -> c_setTupleId aff typ id


foreign import ccall "isl_aff_val_on_domain" c_valOnDomain :: LocalSpace -> Val -> IO Aff

valOnDomain :: forall m. MonadIO m => LocalSpace %1 -> Val %1 -> IslT m Aff
valOnDomain = unsafeCoerce go where
  go :: LocalSpace -> Val -> IslT m Aff
  go ls val =
    unsafeIslFromIO $ \_ -> c_valOnDomain ls val


foreign import ccall "isl_aff_val_on_domain_space" c_valOnDomainSpace :: Space -> Val -> IO Aff

valOnDomainSpace :: forall m. MonadIO m => Space %1 -> Val %1 -> IslT m Aff
valOnDomainSpace = unsafeCoerce go where
  go :: Space -> Val -> IslT m Aff
  go space val =
    unsafeIslFromIO $ \_ -> c_valOnDomainSpace space val


foreign import ccall "isl_aff_var_on_domain" c_varOnDomain :: LocalSpace -> DimType -> C.CUInt -> IO Aff

varOnDomain :: forall m. MonadIO m => LocalSpace %1 -> DimType -> Int -> IslT m Aff
varOnDomain = unsafeCoerce go where
  go :: LocalSpace -> DimType -> Int -> IslT m Aff
  go ls typ pos =
    unsafeIslFromIO $ \_ -> c_varOnDomain ls typ (fromIntegral pos)


foreign import ccall "isl_aff_zero_on_domain" c_zeroOnDomain :: LocalSpace -> IO Aff

zeroOnDomain :: forall m. MonadIO m => LocalSpace %1 -> IslT m Aff
zeroOnDomain = unsafeCoerce go where
  go :: LocalSpace -> IslT m Aff
  go ls =
    unsafeIslFromIO $ \_ -> c_zeroOnDomain ls


foreign import ccall "isl_aff_zero_on_domain_space" c_zeroOnDomainSpace :: Space -> IO Aff

zeroOnDomainSpace :: forall m. MonadIO m => Space %1 -> IslT m Aff
zeroOnDomainSpace = unsafeCoerce go where
  go :: Space -> IslT m Aff
  go space =
    unsafeIslFromIO $ \_ -> c_zeroOnDomainSpace space


foreign import ccall "isl_aff_to_str" c_toStr :: AffRef -> IO C.CString

toStr :: AffRef -> String
toStr aff =
    let !r = unsafePerformIO $ C.peekCString =<< c_toStr aff in r


foreign import ccall "isl_aff_get_domain_local_space" c_getDomainLocalSpace :: AffRef -> IO LocalSpace

getDomainLocalSpace :: MonadIO m => AffRef -> IslT m LocalSpace
getDomainLocalSpace aff =
    unsafeIslFromIO $ \_ -> c_getDomainLocalSpace aff


foreign import ccall "isl_aff_get_local_space" c_getLocalSpace :: AffRef -> IO LocalSpace

getLocalSpace :: MonadIO m => AffRef -> IslT m LocalSpace
getLocalSpace aff =
    unsafeIslFromIO $ \_ -> c_getLocalSpace aff


foreign import ccall "isl_aff_is_cst" c_isCst :: AffRef -> IO C.CBool

isCst :: AffRef -> Bool
isCst aff =
    let !r = unsafePerformIO $ M.toBool <$> c_isCst aff in r


foreign import ccall "isl_aff_plain_is_equal" c_plainIsEqual :: AffRef -> AffRef -> IO C.CBool

plainIsEqual :: AffRef -> AffRef -> Bool
plainIsEqual aff1 aff2 =
    let !r = unsafePerformIO $ M.toBool <$> c_plainIsEqual aff1 aff2 in r


foreign import ccall "isl_aff_eq_set" c_eqSet :: Aff -> Aff -> IO Set

eqSet :: forall m. MonadIO m => Aff %1 -> Aff %1 -> IslT m Set
eqSet = unsafeCoerce go where
  go :: Aff -> Aff -> IslT m Set
  go aff1 aff2 =
    unsafeIslFromIO $ \_ -> c_eqSet aff1 aff2


foreign import ccall "isl_aff_ge_set" c_geSet :: Aff -> Aff -> IO Set

geSet :: forall m. MonadIO m => Aff %1 -> Aff %1 -> IslT m Set
geSet = unsafeCoerce go where
  go :: Aff -> Aff -> IslT m Set
  go aff1 aff2 =
    unsafeIslFromIO $ \_ -> c_geSet aff1 aff2


foreign import ccall "isl_aff_gt_set" c_gtSet :: Aff -> Aff -> IO Set

gtSet :: forall m. MonadIO m => Aff %1 -> Aff %1 -> IslT m Set
gtSet = unsafeCoerce go where
  go :: Aff -> Aff -> IslT m Set
  go aff1 aff2 =
    unsafeIslFromIO $ \_ -> c_gtSet aff1 aff2


foreign import ccall "isl_aff_le_set" c_leSet :: Aff -> Aff -> IO Set

leSet :: forall m. MonadIO m => Aff %1 -> Aff %1 -> IslT m Set
leSet = unsafeCoerce go where
  go :: Aff -> Aff -> IslT m Set
  go aff1 aff2 =
    unsafeIslFromIO $ \_ -> c_leSet aff1 aff2


foreign import ccall "isl_aff_lt_set" c_ltSet :: Aff -> Aff -> IO Set

ltSet :: forall m. MonadIO m => Aff %1 -> Aff %1 -> IslT m Set
ltSet = unsafeCoerce go where
  go :: Aff -> Aff -> IslT m Set
  go aff1 aff2 =
    unsafeIslFromIO $ \_ -> c_ltSet aff1 aff2


foreign import ccall "isl_aff_ne_set" c_neSet :: Aff -> Aff -> IO Set

neSet :: forall m. MonadIO m => Aff %1 -> Aff %1 -> IslT m Set
neSet = unsafeCoerce go where
  go :: Aff -> Aff -> IslT m Set
  go aff1 aff2 =
    unsafeIslFromIO $ \_ -> c_neSet aff1 aff2


foreign import ccall "isl_aff_get_constant_val" c_getConstantVal :: AffRef -> IO Val

getConstantVal :: MonadIO m => AffRef -> IslT m Val
getConstantVal aff =
    unsafeIslFromIO $ \_ -> c_getConstantVal aff


foreign import ccall "isl_aff_add" c_add :: Aff -> Aff -> IO Aff

add :: forall m. MonadIO m => Aff %1 -> Aff %1 -> IslT m Aff
add = unsafeCoerce go where
  go :: Aff -> Aff -> IslT m Aff
  go aff1 aff2 =
    unsafeIslFromIO $ \_ -> c_add aff1 aff2


foreign import ccall "isl_aff_ceil" c_ceil :: Aff -> IO Aff

ceil :: forall m. MonadIO m => Aff %1 -> IslT m Aff
ceil = unsafeCoerce go where
  go :: Aff -> IslT m Aff
  go aff =
    unsafeIslFromIO $ \_ -> c_ceil aff


foreign import ccall "isl_aff_div" c_div :: Aff -> Aff -> IO Aff

div :: forall m. MonadIO m => Aff %1 -> Aff %1 -> IslT m Aff
div = unsafeCoerce go where
  go :: Aff -> Aff -> IslT m Aff
  go aff1 aff2 =
    unsafeIslFromIO $ \_ -> c_div aff1 aff2


foreign import ccall "isl_aff_domain_reverse" c_domainReverse :: Aff -> IO Aff

domainReverse :: forall m. MonadIO m => Aff %1 -> IslT m Aff
domainReverse = unsafeCoerce go where
  go :: Aff -> IslT m Aff
  go aff =
    unsafeIslFromIO $ \_ -> c_domainReverse aff


foreign import ccall "isl_aff_floor" c_floor :: Aff -> IO Aff

floor :: forall m. MonadIO m => Aff %1 -> IslT m Aff
floor = unsafeCoerce go where
  go :: Aff -> IslT m Aff
  go aff =
    unsafeIslFromIO $ \_ -> c_floor aff


foreign import ccall "isl_aff_gist" c_gist :: Aff -> Set -> IO Aff

gist :: forall m. MonadIO m => Aff %1 -> Set %1 -> IslT m Aff
gist = unsafeCoerce go where
  go :: Aff -> Set -> IslT m Aff
  go aff context =
    unsafeIslFromIO $ \_ -> c_gist aff context


foreign import ccall "isl_aff_gist_params" c_gistParams :: Aff -> Set -> IO Aff

gistParams :: forall m. MonadIO m => Aff %1 -> Set %1 -> IslT m Aff
gistParams = unsafeCoerce go where
  go :: Aff -> Set -> IslT m Aff
  go aff context =
    unsafeIslFromIO $ \_ -> c_gistParams aff context


foreign import ccall "isl_aff_mul" c_mul :: Aff -> Aff -> IO Aff

mul :: forall m. MonadIO m => Aff %1 -> Aff %1 -> IslT m Aff
mul = unsafeCoerce go where
  go :: Aff -> Aff -> IslT m Aff
  go aff1 aff2 =
    unsafeIslFromIO $ \_ -> c_mul aff1 aff2


foreign import ccall "isl_aff_neg" c_neg :: Aff -> IO Aff

neg :: forall m. MonadIO m => Aff %1 -> IslT m Aff
neg = unsafeCoerce go where
  go :: Aff -> IslT m Aff
  go aff =
    unsafeIslFromIO $ \_ -> c_neg aff


foreign import ccall "isl_aff_sub" c_sub :: Aff -> Aff -> IO Aff

sub :: forall m. MonadIO m => Aff %1 -> Aff %1 -> IslT m Aff
sub = unsafeCoerce go where
  go :: Aff -> Aff -> IslT m Aff
  go aff1 aff2 =
    unsafeIslFromIO $ \_ -> c_sub aff1 aff2


foreign import ccall "isl_aff_to_list" c_toList :: Aff -> IO AffList

toList :: forall m. MonadIO m => Aff %1 -> IslT m AffList
toList = unsafeCoerce go where
  go :: Aff -> IslT m AffList
  go el =
    unsafeIslFromIO $ \_ -> c_toList el


foreign import ccall "isl_aff_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO Aff

readFromStr :: MonadIO m => String -> IslT m Aff
readFromStr str =
    unsafeIslFromIO $ \ctx -> do
      str_c <- C.newCString str
      c_readFromStr ctx str_c


foreign import ccall "isl_aff_free" c_free :: Aff -> IO ()

instance Consumable Aff where
  consume = unsafeCoerce $ \x -> unsafePerformIO (c_free x)


foreign import ccall "isl_aff_copy" c_copy :: Aff -> IO Aff

instance Dupable Aff where
  dup = unsafeCoerce $ \x -> unsafePerformIO $ do
    copy <- c_copy x
    return (x, copy)


instance Borrow Aff AffRef where
  borrow = unsafeCoerce $ \(Aff ptr) f -> let !r = f (AffRef ptr) in (r, Aff ptr)


