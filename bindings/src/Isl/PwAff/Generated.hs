{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Isl.PwAff.Generated where

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

foreign import ccall "isl_pw_aff_dim" c_dim :: PwAffRef -> DimType -> IO C.CInt

dim :: PwAffRef -> DimType -> Int
dim pwaff typ =
    let !r = unsafePerformIO $ fromIntegral <$> c_dim pwaff typ in r


foreign import ccall "isl_pw_aff_find_dim_by_name" c_findDimByName :: PwAffRef -> DimType -> C.CString -> IO C.CInt

findDimByName :: PwAffRef -> DimType -> String -> Int
findDimByName pa typ name =
    let !r = unsafePerformIO $ do
          name_c <- C.newCString name
          fromIntegral <$> c_findDimByName pa typ name_c
    in r


foreign import ccall "isl_pw_aff_involves_dims" c_involvesDims :: PwAffRef -> DimType -> C.CUInt -> C.CUInt -> IO C.CInt

involvesDims :: PwAffRef -> DimType -> Int -> Int -> Int
involvesDims pwaff typ first n =
    let !r = unsafePerformIO $ fromIntegral <$> c_involvesDims pwaff typ (fromIntegral first) (fromIntegral n) in r


foreign import ccall "isl_pw_aff_involves_nan" c_involvesNan :: PwAffRef -> IO C.CInt

involvesNan :: PwAffRef -> Int
involvesNan pa =
    let !r = unsafePerformIO $ fromIntegral <$> c_involvesNan pa in r


foreign import ccall "isl_pw_aff_involves_param_id" c_involvesParamId :: PwAffRef -> IdRef -> IO C.CInt

involvesParamId :: PwAffRef -> IdRef -> Int
involvesParamId pa id =
    let !r = unsafePerformIO $ fromIntegral <$> c_involvesParamId pa id in r


foreign import ccall "isl_pw_aff_n_piece" c_nPiece :: PwAffRef -> IO C.CInt

nPiece :: PwAffRef -> Int
nPiece pwaff =
    let !r = unsafePerformIO $ fromIntegral <$> c_nPiece pwaff in r


foreign import ccall "isl_pw_aff_plain_cmp" c_plainCmp :: PwAffRef -> PwAffRef -> IO C.CInt

plainCmp :: PwAffRef -> PwAffRef -> Int
plainCmp pa1 pa2 =
    let !r = unsafePerformIO $ fromIntegral <$> c_plainCmp pa1 pa2 in r


foreign import ccall "isl_pw_aff_dump" c_dump :: PwAffRef -> IO ()

dump :: PwAffRef -> ()
dump pwaff =
    let !r = unsafePerformIO $ c_dump pwaff in r


foreign import ccall "isl_pw_aff_get_dim_name" c_getDimName :: PwAffRef -> DimType -> C.CUInt -> IO C.CString

getDimName :: PwAffRef -> DimType -> Int -> String
getDimName pa typ pos =
    let !r = unsafePerformIO $ C.peekCString =<< c_getDimName pa typ (fromIntegral pos) in r


foreign import ccall "isl_pw_aff_has_dim_id" c_hasDimId :: PwAffRef -> DimType -> C.CUInt -> IO C.CBool

hasDimId :: PwAffRef -> DimType -> Int -> Bool
hasDimId pa typ pos =
    let !r = unsafePerformIO $ M.toBool <$> c_hasDimId pa typ (fromIntegral pos) in r


foreign import ccall "isl_pw_aff_has_tuple_id" c_hasTupleId :: PwAffRef -> DimType -> IO C.CBool

hasTupleId :: PwAffRef -> DimType -> Bool
hasTupleId pa typ =
    let !r = unsafePerformIO $ M.toBool <$> c_hasTupleId pa typ in r


foreign import ccall "isl_pw_aff_is_cst" c_isCst :: PwAffRef -> IO C.CBool

isCst :: PwAffRef -> Bool
isCst pwaff =
    let !r = unsafePerformIO $ M.toBool <$> c_isCst pwaff in r


foreign import ccall "isl_pw_aff_is_empty" c_isEmpty :: PwAffRef -> IO C.CBool

isEmpty :: PwAffRef -> Bool
isEmpty pwaff =
    let !r = unsafePerformIO $ M.toBool <$> c_isEmpty pwaff in r


foreign import ccall "isl_pw_aff_is_equal" c_isEqual :: PwAffRef -> PwAffRef -> IO C.CBool

isEqual :: PwAffRef -> PwAffRef -> Bool
isEqual pa1 pa2 =
    let !r = unsafePerformIO $ M.toBool <$> c_isEqual pa1 pa2 in r


foreign import ccall "isl_pw_aff_bind_id" c_bindId :: PwAff -> Id -> IO Set

bindId :: forall m. MonadIO m => PwAff %1 -> Id %1 -> IslT m Set
bindId = unsafeCoerce go where
  go :: PwAff -> Id -> IslT m Set
  go pa id =
    unsafeIslFromIO $ \_ -> c_bindId pa id


foreign import ccall "isl_pw_aff_non_zero_set" c_nonZeroSet :: PwAff -> IO Set

nonZeroSet :: forall m. MonadIO m => PwAff %1 -> IslT m Set
nonZeroSet = unsafeCoerce go where
  go :: PwAff -> IslT m Set
  go pwaff =
    unsafeIslFromIO $ \_ -> c_nonZeroSet pwaff


foreign import ccall "isl_pw_aff_nonneg_set" c_nonnegSet :: PwAff -> IO Set

nonnegSet :: forall m. MonadIO m => PwAff %1 -> IslT m Set
nonnegSet = unsafeCoerce go where
  go :: PwAff -> IslT m Set
  go pwaff =
    unsafeIslFromIO $ \_ -> c_nonnegSet pwaff


foreign import ccall "isl_pw_aff_pos_set" c_posSet :: PwAff -> IO Set

posSet :: forall m. MonadIO m => PwAff %1 -> IslT m Set
posSet = unsafeCoerce go where
  go :: PwAff -> IslT m Set
  go pa =
    unsafeIslFromIO $ \_ -> c_posSet pa


foreign import ccall "isl_pw_aff_zero_set" c_zeroSet :: PwAff -> IO Set

zeroSet :: forall m. MonadIO m => PwAff %1 -> IslT m Set
zeroSet = unsafeCoerce go where
  go :: PwAff -> IslT m Set
  go pwaff =
    unsafeIslFromIO $ \_ -> c_zeroSet pwaff


foreign import ccall "isl_pw_aff_get_domain_space" c_getDomainSpace :: PwAffRef -> IO Space

getDomainSpace :: MonadIO m => PwAffRef -> IslT m Space
getDomainSpace pwaff =
    unsafeIslFromIO $ \_ -> c_getDomainSpace pwaff


foreign import ccall "isl_pw_aff_eq_map" c_eqMap :: PwAff -> PwAff -> IO Map

eqMap :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m Map
eqMap = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m Map
  go pa1 pa2 =
    unsafeIslFromIO $ \_ -> c_eqMap pa1 pa2


foreign import ccall "isl_pw_aff_ge_map" c_geMap :: PwAff -> PwAff -> IO Map

geMap :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m Map
geMap = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m Map
  go pa1 pa2 =
    unsafeIslFromIO $ \_ -> c_geMap pa1 pa2


foreign import ccall "isl_pw_aff_gt_map" c_gtMap :: PwAff -> PwAff -> IO Map

gtMap :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m Map
gtMap = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m Map
  go pa1 pa2 =
    unsafeIslFromIO $ \_ -> c_gtMap pa1 pa2


foreign import ccall "isl_pw_aff_le_map" c_leMap :: PwAff -> PwAff -> IO Map

leMap :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m Map
leMap = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m Map
  go pa1 pa2 =
    unsafeIslFromIO $ \_ -> c_leMap pa1 pa2


foreign import ccall "isl_pw_aff_lt_map" c_ltMap :: PwAff -> PwAff -> IO Map

ltMap :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m Map
ltMap = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m Map
  go pa1 pa2 =
    unsafeIslFromIO $ \_ -> c_ltMap pa1 pa2


foreign import ccall "isl_pw_aff_add_constant_val" c_addConstantVal :: PwAff -> Val -> IO PwAff

addConstantVal :: forall m. MonadIO m => PwAff %1 -> Val %1 -> IslT m PwAff
addConstantVal = unsafeCoerce go where
  go :: PwAff -> Val -> IslT m PwAff
  go pa v =
    unsafeIslFromIO $ \_ -> c_addConstantVal pa v


foreign import ccall "isl_pw_aff_add_dims" c_addDims :: PwAff -> DimType -> C.CUInt -> IO PwAff

addDims :: forall m. MonadIO m => PwAff %1 -> DimType -> Int -> IslT m PwAff
addDims = unsafeCoerce go where
  go :: PwAff -> DimType -> Int -> IslT m PwAff
  go pwaff typ n =
    unsafeIslFromIO $ \_ -> c_addDims pwaff typ (fromIntegral n)


foreign import ccall "isl_pw_aff_align_params" c_alignParams :: PwAff -> Space -> IO PwAff

alignParams :: forall m. MonadIO m => PwAff %1 -> Space %1 -> IslT m PwAff
alignParams = unsafeCoerce go where
  go :: PwAff -> Space -> IslT m PwAff
  go pwaff model =
    unsafeIslFromIO $ \_ -> c_alignParams pwaff model


foreign import ccall "isl_pw_aff_alloc" c_alloc :: Set -> Aff -> IO PwAff

alloc :: forall m. MonadIO m => Set %1 -> Aff %1 -> IslT m PwAff
alloc = unsafeCoerce go where
  go :: Set -> Aff -> IslT m PwAff
  go set aff =
    unsafeIslFromIO $ \_ -> c_alloc set aff


foreign import ccall "isl_pw_aff_drop_dims" c_dropDims :: PwAff -> DimType -> C.CUInt -> C.CUInt -> IO PwAff

dropDims :: forall m. MonadIO m => PwAff %1 -> DimType -> Int -> Int -> IslT m PwAff
dropDims = unsafeCoerce go where
  go :: PwAff -> DimType -> Int -> Int -> IslT m PwAff
  go pwaff typ first n =
    unsafeIslFromIO $ \_ -> c_dropDims pwaff typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_pw_aff_empty" c_empty :: Space -> IO PwAff

empty :: forall m. MonadIO m => Space %1 -> IslT m PwAff
empty = unsafeCoerce go where
  go :: Space -> IslT m PwAff
  go space =
    unsafeIslFromIO $ \_ -> c_empty space


foreign import ccall "isl_pw_aff_from_range" c_fromRange :: PwAff -> IO PwAff

fromRange :: forall m. MonadIO m => PwAff %1 -> IslT m PwAff
fromRange = unsafeCoerce go where
  go :: PwAff -> IslT m PwAff
  go pwa =
    unsafeIslFromIO $ \_ -> c_fromRange pwa


foreign import ccall "isl_pw_aff_insert_dims" c_insertDims :: PwAff -> DimType -> C.CUInt -> C.CUInt -> IO PwAff

insertDims :: forall m. MonadIO m => PwAff %1 -> DimType -> Int -> Int -> IslT m PwAff
insertDims = unsafeCoerce go where
  go :: PwAff -> DimType -> Int -> Int -> IslT m PwAff
  go pwaff typ first n =
    unsafeIslFromIO $ \_ -> c_insertDims pwaff typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_pw_aff_intersect_domain_wrapped_domain" c_intersectDomainWrappedDomain :: PwAff -> Set -> IO PwAff

intersectDomainWrappedDomain :: forall m. MonadIO m => PwAff %1 -> Set %1 -> IslT m PwAff
intersectDomainWrappedDomain = unsafeCoerce go where
  go :: PwAff -> Set -> IslT m PwAff
  go pa set =
    unsafeIslFromIO $ \_ -> c_intersectDomainWrappedDomain pa set


foreign import ccall "isl_pw_aff_intersect_domain_wrapped_range" c_intersectDomainWrappedRange :: PwAff -> Set -> IO PwAff

intersectDomainWrappedRange :: forall m. MonadIO m => PwAff %1 -> Set %1 -> IslT m PwAff
intersectDomainWrappedRange = unsafeCoerce go where
  go :: PwAff -> Set -> IslT m PwAff
  go pa set =
    unsafeIslFromIO $ \_ -> c_intersectDomainWrappedRange pa set


foreign import ccall "isl_pw_aff_mod_val" c_modVal :: PwAff -> ValRef -> IO PwAff

modVal :: forall m. MonadIO m => PwAff %1 -> ValRef -> IslT m PwAff
modVal = unsafeCoerce go where
  go :: PwAff -> ValRef -> IslT m PwAff
  go pa modulo =
    unsafeIslFromIO $ \_ -> c_modVal pa modulo


foreign import ccall "isl_pw_aff_move_dims" c_moveDims :: PwAff -> DimType -> C.CUInt -> DimType -> C.CUInt -> C.CUInt -> IO PwAff

moveDims :: forall m. MonadIO m => PwAff %1 -> DimType -> Int -> DimType -> Int -> Int -> IslT m PwAff
moveDims = unsafeCoerce go where
  go :: PwAff -> DimType -> Int -> DimType -> Int -> Int -> IslT m PwAff
  go pa dst_type dst_pos src_type src_pos n =
    unsafeIslFromIO $ \_ -> c_moveDims pa dst_type (fromIntegral dst_pos) src_type (fromIntegral src_pos) (fromIntegral n)


foreign import ccall "isl_pw_aff_nan_on_domain" c_nanOnDomain :: LocalSpace -> IO PwAff

nanOnDomain :: forall m. MonadIO m => LocalSpace %1 -> IslT m PwAff
nanOnDomain = unsafeCoerce go where
  go :: LocalSpace -> IslT m PwAff
  go ls =
    unsafeIslFromIO $ \_ -> c_nanOnDomain ls


foreign import ccall "isl_pw_aff_nan_on_domain_space" c_nanOnDomainSpace :: Space -> IO PwAff

nanOnDomainSpace :: forall m. MonadIO m => Space %1 -> IslT m PwAff
nanOnDomainSpace = unsafeCoerce go where
  go :: Space -> IslT m PwAff
  go space =
    unsafeIslFromIO $ \_ -> c_nanOnDomainSpace space


foreign import ccall "isl_pw_aff_param_on_domain_id" c_paramOnDomainId :: Set -> Id -> IO PwAff

paramOnDomainId :: forall m. MonadIO m => Set %1 -> Id %1 -> IslT m PwAff
paramOnDomainId = unsafeCoerce go where
  go :: Set -> Id -> IslT m PwAff
  go domain id =
    unsafeIslFromIO $ \_ -> c_paramOnDomainId domain id


foreign import ccall "isl_pw_aff_project_domain_on_params" c_projectDomainOnParams :: PwAff -> IO PwAff

projectDomainOnParams :: forall m. MonadIO m => PwAff %1 -> IslT m PwAff
projectDomainOnParams = unsafeCoerce go where
  go :: PwAff -> IslT m PwAff
  go pa =
    unsafeIslFromIO $ \_ -> c_projectDomainOnParams pa


foreign import ccall "isl_pw_aff_pullback_multi_aff" c_pullbackMultiAff :: PwAff -> MultiAff -> IO PwAff

pullbackMultiAff :: forall m. MonadIO m => PwAff %1 -> MultiAff %1 -> IslT m PwAff
pullbackMultiAff = unsafeCoerce go where
  go :: PwAff -> MultiAff -> IslT m PwAff
  go pa ma =
    unsafeIslFromIO $ \_ -> c_pullbackMultiAff pa ma


foreign import ccall "isl_pw_aff_pullback_pw_multi_aff" c_pullbackPwMultiAff :: PwAff -> PwMultiAff -> IO PwAff

pullbackPwMultiAff :: forall m. MonadIO m => PwAff %1 -> PwMultiAff %1 -> IslT m PwAff
pullbackPwMultiAff = unsafeCoerce go where
  go :: PwAff -> PwMultiAff -> IslT m PwAff
  go pa pma =
    unsafeIslFromIO $ \_ -> c_pullbackPwMultiAff pa pma


foreign import ccall "isl_pw_aff_reset_tuple_id" c_resetTupleId :: PwAff -> DimType -> IO PwAff

resetTupleId :: forall m. MonadIO m => PwAff %1 -> DimType -> IslT m PwAff
resetTupleId = unsafeCoerce go where
  go :: PwAff -> DimType -> IslT m PwAff
  go pa typ =
    unsafeIslFromIO $ \_ -> c_resetTupleId pa typ


foreign import ccall "isl_pw_aff_reset_user" c_resetUser :: PwAff -> IO PwAff

resetUser :: forall m. MonadIO m => PwAff %1 -> IslT m PwAff
resetUser = unsafeCoerce go where
  go :: PwAff -> IslT m PwAff
  go pa =
    unsafeIslFromIO $ \_ -> c_resetUser pa


foreign import ccall "isl_pw_aff_scale_down_val" c_scaleDownVal :: PwAff -> Val -> IO PwAff

scaleDownVal :: forall m. MonadIO m => PwAff %1 -> Val %1 -> IslT m PwAff
scaleDownVal = unsafeCoerce go where
  go :: PwAff -> Val -> IslT m PwAff
  go pa f =
    unsafeIslFromIO $ \_ -> c_scaleDownVal pa f


foreign import ccall "isl_pw_aff_scale_val" c_scaleVal :: PwAff -> Val -> IO PwAff

scaleVal :: forall m. MonadIO m => PwAff %1 -> Val %1 -> IslT m PwAff
scaleVal = unsafeCoerce go where
  go :: PwAff -> Val -> IslT m PwAff
  go pa v =
    unsafeIslFromIO $ \_ -> c_scaleVal pa v


foreign import ccall "isl_pw_aff_set_dim_id" c_setDimId :: PwAff -> DimType -> C.CUInt -> Id -> IO PwAff

setDimId :: forall m. MonadIO m => PwAff %1 -> DimType -> Int -> Id %1 -> IslT m PwAff
setDimId = unsafeCoerce go where
  go :: PwAff -> DimType -> Int -> Id -> IslT m PwAff
  go pma typ pos id =
    unsafeIslFromIO $ \_ -> c_setDimId pma typ (fromIntegral pos) id


foreign import ccall "isl_pw_aff_set_tuple_id" c_setTupleId :: PwAff -> DimType -> Id -> IO PwAff

setTupleId :: forall m. MonadIO m => PwAff %1 -> DimType -> Id %1 -> IslT m PwAff
setTupleId = unsafeCoerce go where
  go :: PwAff -> DimType -> Id -> IslT m PwAff
  go pwaff typ id =
    unsafeIslFromIO $ \_ -> c_setTupleId pwaff typ id


foreign import ccall "isl_pw_aff_union_max" c_unionMax :: PwAff -> PwAff -> IO PwAff

unionMax :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m PwAff
unionMax = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m PwAff
  go pwaff1 pwaff2 =
    unsafeIslFromIO $ \_ -> c_unionMax pwaff1 pwaff2


foreign import ccall "isl_pw_aff_union_min" c_unionMin :: PwAff -> PwAff -> IO PwAff

unionMin :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m PwAff
unionMin = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m PwAff
  go pwaff1 pwaff2 =
    unsafeIslFromIO $ \_ -> c_unionMin pwaff1 pwaff2


foreign import ccall "isl_pw_aff_val_on_domain" c_valOnDomain :: Set -> Val -> IO PwAff

valOnDomain :: forall m. MonadIO m => Set %1 -> Val %1 -> IslT m PwAff
valOnDomain = unsafeCoerce go where
  go :: Set -> Val -> IslT m PwAff
  go domain v =
    unsafeIslFromIO $ \_ -> c_valOnDomain domain v


foreign import ccall "isl_pw_aff_var_on_domain" c_varOnDomain :: LocalSpace -> DimType -> C.CUInt -> IO PwAff

varOnDomain :: forall m. MonadIO m => LocalSpace %1 -> DimType -> Int -> IslT m PwAff
varOnDomain = unsafeCoerce go where
  go :: LocalSpace -> DimType -> Int -> IslT m PwAff
  go ls typ pos =
    unsafeIslFromIO $ \_ -> c_varOnDomain ls typ (fromIntegral pos)


foreign import ccall "isl_pw_aff_zero_on_domain" c_zeroOnDomain :: LocalSpace -> IO PwAff

zeroOnDomain :: forall m. MonadIO m => LocalSpace %1 -> IslT m PwAff
zeroOnDomain = unsafeCoerce go where
  go :: LocalSpace -> IslT m PwAff
  go ls =
    unsafeIslFromIO $ \_ -> c_zeroOnDomain ls


foreign import ccall "isl_pw_aff_get_dim_id" c_getDimId :: PwAffRef -> DimType -> C.CUInt -> IO Id

getDimId :: MonadIO m => PwAffRef -> DimType -> Int -> IslT m Id
getDimId pa typ pos =
    unsafeIslFromIO $ \_ -> c_getDimId pa typ (fromIntegral pos)


foreign import ccall "isl_pw_aff_get_tuple_id" c_getTupleId :: PwAffRef -> DimType -> IO Id

getTupleId :: MonadIO m => PwAffRef -> DimType -> IslT m Id
getTupleId pa typ =
    unsafeIslFromIO $ \_ -> c_getTupleId pa typ


foreign import ccall "isl_pw_aff_to_str" c_toStr :: PwAffRef -> IO C.CString

toStr :: PwAffRef -> String
toStr pa =
    let !r = unsafePerformIO $ C.peekCString =<< c_toStr pa in r


foreign import ccall "isl_pw_aff_isa_aff" c_isaAff :: PwAffRef -> IO C.CInt

isaAff :: PwAffRef -> Int
isaAff pa =
    let !r = unsafePerformIO $ fromIntegral <$> c_isaAff pa in r


foreign import ccall "isl_pw_aff_plain_is_equal" c_plainIsEqual :: PwAffRef -> PwAffRef -> IO C.CBool

plainIsEqual :: PwAffRef -> PwAffRef -> Bool
plainIsEqual pwaff1 pwaff2 =
    let !r = unsafePerformIO $ M.toBool <$> c_plainIsEqual pwaff1 pwaff2 in r


foreign import ccall "isl_pw_aff_domain" c_domain :: PwAff -> IO Set

domain :: forall m. MonadIO m => PwAff %1 -> IslT m Set
domain = unsafeCoerce go where
  go :: PwAff -> IslT m Set
  go pwaff =
    unsafeIslFromIO $ \_ -> c_domain pwaff


foreign import ccall "isl_pw_aff_eq_set" c_eqSet :: PwAff -> PwAff -> IO Set

eqSet :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m Set
eqSet = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m Set
  go pwaff1 pwaff2 =
    unsafeIslFromIO $ \_ -> c_eqSet pwaff1 pwaff2


foreign import ccall "isl_pw_aff_ge_set" c_geSet :: PwAff -> PwAff -> IO Set

geSet :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m Set
geSet = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m Set
  go pwaff1 pwaff2 =
    unsafeIslFromIO $ \_ -> c_geSet pwaff1 pwaff2


foreign import ccall "isl_pw_aff_gt_set" c_gtSet :: PwAff -> PwAff -> IO Set

gtSet :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m Set
gtSet = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m Set
  go pwaff1 pwaff2 =
    unsafeIslFromIO $ \_ -> c_gtSet pwaff1 pwaff2


foreign import ccall "isl_pw_aff_le_set" c_leSet :: PwAff -> PwAff -> IO Set

leSet :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m Set
leSet = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m Set
  go pwaff1 pwaff2 =
    unsafeIslFromIO $ \_ -> c_leSet pwaff1 pwaff2


foreign import ccall "isl_pw_aff_lt_set" c_ltSet :: PwAff -> PwAff -> IO Set

ltSet :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m Set
ltSet = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m Set
  go pwaff1 pwaff2 =
    unsafeIslFromIO $ \_ -> c_ltSet pwaff1 pwaff2


foreign import ccall "isl_pw_aff_ne_set" c_neSet :: PwAff -> PwAff -> IO Set

neSet :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m Set
neSet = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m Set
  go pwaff1 pwaff2 =
    unsafeIslFromIO $ \_ -> c_neSet pwaff1 pwaff2


foreign import ccall "isl_pw_aff_params" c_params :: PwAff -> IO Set

params :: forall m. MonadIO m => PwAff %1 -> IslT m Set
params = unsafeCoerce go where
  go :: PwAff -> IslT m Set
  go pwa =
    unsafeIslFromIO $ \_ -> c_params pwa


foreign import ccall "isl_pw_aff_get_space" c_getSpace :: PwAffRef -> IO Space

getSpace :: MonadIO m => PwAffRef -> IslT m Space
getSpace pwaff =
    unsafeIslFromIO $ \_ -> c_getSpace pwaff


foreign import ccall "isl_pw_aff_as_map" c_asMap :: PwAff -> IO Map

asMap :: forall m. MonadIO m => PwAff %1 -> IslT m Map
asMap = unsafeCoerce go where
  go :: PwAff -> IslT m Map
  go pa =
    unsafeIslFromIO $ \_ -> c_asMap pa


foreign import ccall "isl_pw_aff_max_val" c_maxVal :: PwAff -> IO Val

maxVal :: forall m. MonadIO m => PwAff %1 -> IslT m Val
maxVal = unsafeCoerce go where
  go :: PwAff -> IslT m Val
  go pa =
    unsafeIslFromIO $ \_ -> c_maxVal pa


foreign import ccall "isl_pw_aff_min_val" c_minVal :: PwAff -> IO Val

minVal :: forall m. MonadIO m => PwAff %1 -> IslT m Val
minVal = unsafeCoerce go where
  go :: PwAff -> IslT m Val
  go pa =
    unsafeIslFromIO $ \_ -> c_minVal pa


foreign import ccall "isl_pw_aff_add" c_add :: PwAff -> PwAff -> IO PwAff

add :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m PwAff
add = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m PwAff
  go pwaff1 pwaff2 =
    unsafeIslFromIO $ \_ -> c_add pwaff1 pwaff2


foreign import ccall "isl_pw_aff_ceil" c_ceil :: PwAff -> IO PwAff

ceil :: forall m. MonadIO m => PwAff %1 -> IslT m PwAff
ceil = unsafeCoerce go where
  go :: PwAff -> IslT m PwAff
  go pwaff =
    unsafeIslFromIO $ \_ -> c_ceil pwaff


foreign import ccall "isl_pw_aff_coalesce" c_coalesce :: PwAff -> IO PwAff

coalesce :: forall m. MonadIO m => PwAff %1 -> IslT m PwAff
coalesce = unsafeCoerce go where
  go :: PwAff -> IslT m PwAff
  go pa =
    unsafeIslFromIO $ \_ -> c_coalesce pa


foreign import ccall "isl_pw_aff_cond" c_cond :: PwAff -> PwAff -> PwAff -> IO PwAff

cond :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> PwAff %1 -> IslT m PwAff
cond = unsafeCoerce go where
  go :: PwAff -> PwAff -> PwAff -> IslT m PwAff
  go cond pwaff_true pwaff_false =
    unsafeIslFromIO $ \_ -> c_cond cond pwaff_true pwaff_false


foreign import ccall "isl_pw_aff_div" c_div :: PwAff -> PwAff -> IO PwAff

div :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m PwAff
div = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m PwAff
  go pa1 pa2 =
    unsafeIslFromIO $ \_ -> c_div pa1 pa2


foreign import ccall "isl_pw_aff_domain_reverse" c_domainReverse :: PwAff -> IO PwAff

domainReverse :: forall m. MonadIO m => PwAff %1 -> IslT m PwAff
domainReverse = unsafeCoerce go where
  go :: PwAff -> IslT m PwAff
  go pa =
    unsafeIslFromIO $ \_ -> c_domainReverse pa


foreign import ccall "isl_pw_aff_drop_unused_params" c_dropUnusedParams :: PwAff -> IO PwAff

dropUnusedParams :: forall m. MonadIO m => PwAff %1 -> IslT m PwAff
dropUnusedParams = unsafeCoerce go where
  go :: PwAff -> IslT m PwAff
  go pa =
    unsafeIslFromIO $ \_ -> c_dropUnusedParams pa


foreign import ccall "isl_pw_aff_floor" c_floor :: PwAff -> IO PwAff

floor :: forall m. MonadIO m => PwAff %1 -> IslT m PwAff
floor = unsafeCoerce go where
  go :: PwAff -> IslT m PwAff
  go pwaff =
    unsafeIslFromIO $ \_ -> c_floor pwaff


foreign import ccall "isl_pw_aff_gist" c_gist :: PwAff -> Set -> IO PwAff

gist :: forall m. MonadIO m => PwAff %1 -> Set %1 -> IslT m PwAff
gist = unsafeCoerce go where
  go :: PwAff -> Set -> IslT m PwAff
  go pwaff context =
    unsafeIslFromIO $ \_ -> c_gist pwaff context


foreign import ccall "isl_pw_aff_gist_params" c_gistParams :: PwAff -> Set -> IO PwAff

gistParams :: forall m. MonadIO m => PwAff %1 -> Set %1 -> IslT m PwAff
gistParams = unsafeCoerce go where
  go :: PwAff -> Set -> IslT m PwAff
  go pwaff context =
    unsafeIslFromIO $ \_ -> c_gistParams pwaff context


foreign import ccall "isl_pw_aff_insert_domain" c_insertDomain :: PwAff -> Space -> IO PwAff

insertDomain :: forall m. MonadIO m => PwAff %1 -> Space %1 -> IslT m PwAff
insertDomain = unsafeCoerce go where
  go :: PwAff -> Space -> IslT m PwAff
  go pa domain =
    unsafeIslFromIO $ \_ -> c_insertDomain pa domain


foreign import ccall "isl_pw_aff_intersect_domain" c_intersectDomain :: PwAff -> Set -> IO PwAff

intersectDomain :: forall m. MonadIO m => PwAff %1 -> Set %1 -> IslT m PwAff
intersectDomain = unsafeCoerce go where
  go :: PwAff -> Set -> IslT m PwAff
  go pa set =
    unsafeIslFromIO $ \_ -> c_intersectDomain pa set


foreign import ccall "isl_pw_aff_intersect_params" c_intersectParams :: PwAff -> Set -> IO PwAff

intersectParams :: forall m. MonadIO m => PwAff %1 -> Set %1 -> IslT m PwAff
intersectParams = unsafeCoerce go where
  go :: PwAff -> Set -> IslT m PwAff
  go pa set =
    unsafeIslFromIO $ \_ -> c_intersectParams pa set


foreign import ccall "isl_pw_aff_max" c_max :: PwAff -> PwAff -> IO PwAff

max :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m PwAff
max = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m PwAff
  go pwaff1 pwaff2 =
    unsafeIslFromIO $ \_ -> c_max pwaff1 pwaff2


foreign import ccall "isl_pw_aff_min" c_min :: PwAff -> PwAff -> IO PwAff

min :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m PwAff
min = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m PwAff
  go pwaff1 pwaff2 =
    unsafeIslFromIO $ \_ -> c_min pwaff1 pwaff2


foreign import ccall "isl_pw_aff_mul" c_mul :: PwAff -> PwAff -> IO PwAff

mul :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m PwAff
mul = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m PwAff
  go pwaff1 pwaff2 =
    unsafeIslFromIO $ \_ -> c_mul pwaff1 pwaff2


foreign import ccall "isl_pw_aff_neg" c_neg :: PwAff -> IO PwAff

neg :: forall m. MonadIO m => PwAff %1 -> IslT m PwAff
neg = unsafeCoerce go where
  go :: PwAff -> IslT m PwAff
  go pwaff =
    unsafeIslFromIO $ \_ -> c_neg pwaff


foreign import ccall "isl_pw_aff_sub" c_sub :: PwAff -> PwAff -> IO PwAff

sub :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m PwAff
sub = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m PwAff
  go pwaff1 pwaff2 =
    unsafeIslFromIO $ \_ -> c_sub pwaff1 pwaff2


foreign import ccall "isl_pw_aff_subtract_domain" c_subtractDomain :: PwAff -> Set -> IO PwAff

subtractDomain :: forall m. MonadIO m => PwAff %1 -> Set %1 -> IslT m PwAff
subtractDomain = unsafeCoerce go where
  go :: PwAff -> Set -> IslT m PwAff
  go pa set =
    unsafeIslFromIO $ \_ -> c_subtractDomain pa set


foreign import ccall "isl_pw_aff_tdiv_q" c_tdivQ :: PwAff -> PwAff -> IO PwAff

tdivQ :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m PwAff
tdivQ = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m PwAff
  go pa1 pa2 =
    unsafeIslFromIO $ \_ -> c_tdivQ pa1 pa2


foreign import ccall "isl_pw_aff_tdiv_r" c_tdivR :: PwAff -> PwAff -> IO PwAff

tdivR :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m PwAff
tdivR = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m PwAff
  go pa1 pa2 =
    unsafeIslFromIO $ \_ -> c_tdivR pa1 pa2


foreign import ccall "isl_pw_aff_union_add" c_unionAdd :: PwAff -> PwAff -> IO PwAff

unionAdd :: forall m. MonadIO m => PwAff %1 -> PwAff %1 -> IslT m PwAff
unionAdd = unsafeCoerce go where
  go :: PwAff -> PwAff -> IslT m PwAff
  go pwaff1 pwaff2 =
    unsafeIslFromIO $ \_ -> c_unionAdd pwaff1 pwaff2


foreign import ccall "isl_pw_aff_as_aff" c_asAff :: PwAff -> IO Aff

asAff :: forall m. MonadIO m => PwAff %1 -> IslT m Aff
asAff = unsafeCoerce go where
  go :: PwAff -> IslT m Aff
  go pa =
    unsafeIslFromIO $ \_ -> c_asAff pa


foreign import ccall "isl_pw_aff_from_aff" c_fromAff :: Aff -> IO PwAff

fromAff :: forall m. MonadIO m => Aff %1 -> IslT m PwAff
fromAff = unsafeCoerce go where
  go :: Aff -> IslT m PwAff
  go aff =
    unsafeIslFromIO $ \_ -> c_fromAff aff


foreign import ccall "isl_pw_aff_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO PwAff

readFromStr :: MonadIO m => String -> IslT m PwAff
readFromStr str =
    unsafeIslFromIO $ \ctx -> do
      str_c <- C.newCString str
      c_readFromStr ctx str_c


foreign import ccall "isl_pw_aff_free" c_free :: PwAff -> IO ()

instance Consumable PwAff where
  consume = unsafeCoerce $ \x -> unsafePerformIO (c_free x)


foreign import ccall "isl_pw_aff_copy" c_copy :: PwAff -> IO PwAff

instance Dupable PwAff where
  dup = unsafeCoerce $ \x -> unsafePerformIO $ do
    copy <- c_copy x
    return (x, copy)


instance Borrow PwAff PwAffRef where
  borrow = unsafeCoerce $ \(PwAff ptr) f -> let !r = f (PwAffRef ptr) in (r, PwAff ptr)


