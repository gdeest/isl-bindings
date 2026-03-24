{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict #-}

module Isl.Aff.AutoGen where

import Control.Monad
import Data.Reflection
import Isl.Types
import Debug.Trace

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce

foreign import ccall "isl_aff_coefficient_sgn" c_coefficientSgn :: Aff -> DimType -> C.CInt -> IO C.CInt


coefficientSgn :: (Given Ctx) => Aff -> DimType -> Int -> Int
coefficientSgn = \aff' typ' pos' -> trace "coefficientSgn" $ 
    unsafePerformIO $ (return . fromIntegral) =<< do
      aff <- (return) aff'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_coefficientSgn aff typ pos


foreign import ccall "isl_aff_find_dim_by_name" c_findDimByName :: Aff -> DimType -> C.CString -> IO C.CInt


findDimByName :: (Given Ctx) => Aff -> DimType -> String -> Int
findDimByName = \aff' typ' name' -> trace "findDimByName" $ 
    unsafePerformIO $ (return . fromIntegral) =<< do
      aff <- (return) aff'
      typ <- (return) typ'
      name <- (C.newCString) name'

      let ctx = given :: Ctx
      c_findDimByName aff typ name


foreign import ccall "isl_aff_involves_dims" c_involvesDims :: Aff -> DimType -> C.CUInt -> C.CUInt -> IO C.CInt


involvesDims :: (Given Ctx) => Aff -> DimType -> Int -> Int -> Int
involvesDims = \aff' typ' first' n' -> trace "involvesDims" $ 
    unsafePerformIO $ (return . fromIntegral) =<< do
      aff <- (return) aff'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_involvesDims aff typ first n


foreign import ccall "isl_aff_involves_locals" c_involvesLocals :: Aff -> IO C.CInt


involvesLocals :: (Given Ctx) => Aff -> Int
involvesLocals = \aff' -> trace "involvesLocals" $ 
    unsafePerformIO $ (return . fromIntegral) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_involvesLocals aff


foreign import ccall "isl_aff_get_ctx" c_getCtx :: Aff -> IO Ctx


getCtx :: (Given Ctx) => Aff -> Ctx
getCtx = \aff' -> trace "getCtx" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_getCtx aff


foreign import ccall "isl_aff_dump" c_dump :: Aff -> IO ()


dump :: (Given Ctx) => Aff -> ()
dump = \aff' -> trace "dump" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_dump aff


foreign import ccall "isl_aff_get_dim_name" c_getDimName :: Aff -> DimType -> C.CUInt -> IO C.CString


getDimName :: (Given Ctx) => Aff -> DimType -> Int -> String
getDimName = \aff' typ' pos' -> trace "getDimName" $ 
    unsafePerformIO $ (C.peekCString) =<< do
      aff <- (return) aff'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDimName aff typ pos


foreign import ccall "isl_aff_is_nan" c_isNan :: Aff -> IO C.CBool


isNan :: (Given Ctx) => Aff -> Bool
isNan = \aff' -> trace "isNan" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_isNan aff


foreign import ccall "isl_aff_plain_is_zero" c_plainIsZero :: Aff -> IO C.CBool


plainIsZero :: (Given Ctx) => Aff -> Bool
plainIsZero = \aff' -> trace "plainIsZero" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_plainIsZero aff


foreign import ccall "isl_aff_get_domain_space" c_getDomainSpace :: Aff -> IO Space


getDomainSpace :: (Given Ctx) => Aff -> Space
getDomainSpace = \aff' -> trace "getDomainSpace" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_getDomainSpace aff


foreign import ccall "isl_aff_get_space" c_getSpace :: Aff -> IO Space


getSpace :: (Given Ctx) => Aff -> Space
getSpace = \aff' -> trace "getSpace" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_getSpace aff


foreign import ccall "isl_aff_bind_id" c_bindId :: Aff -> Id -> IO BasicSet


bindId :: (Given Ctx) => Aff -> Id -> BasicSet
bindId = \aff' id' -> trace "bindId" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      id <- (return) id'

      let ctx = given :: Ctx
      c_bindId aff id


foreign import ccall "isl_aff_eq_basic_set" c_eqBasicSet :: Aff -> Aff -> IO BasicSet


eqBasicSet :: (Given Ctx) => Aff -> Aff -> BasicSet
eqBasicSet = \aff1' aff2' -> trace "eqBasicSet" $ 
    unsafePerformIO $ (return) =<< do
      aff1 <- (return) aff1'
      aff2 <- (return) aff2'

      let ctx = given :: Ctx
      c_eqBasicSet aff1 aff2


foreign import ccall "isl_aff_ge_basic_set" c_geBasicSet :: Aff -> Aff -> IO BasicSet


geBasicSet :: (Given Ctx) => Aff -> Aff -> BasicSet
geBasicSet = \aff1' aff2' -> trace "geBasicSet" $ 
    unsafePerformIO $ (return) =<< do
      aff1 <- (return) aff1'
      aff2 <- (return) aff2'

      let ctx = given :: Ctx
      c_geBasicSet aff1 aff2


foreign import ccall "isl_aff_gt_basic_set" c_gtBasicSet :: Aff -> Aff -> IO BasicSet


gtBasicSet :: (Given Ctx) => Aff -> Aff -> BasicSet
gtBasicSet = \aff1' aff2' -> trace "gtBasicSet" $ 
    unsafePerformIO $ (return) =<< do
      aff1 <- (return) aff1'
      aff2 <- (return) aff2'

      let ctx = given :: Ctx
      c_gtBasicSet aff1 aff2


foreign import ccall "isl_aff_le_basic_set" c_leBasicSet :: Aff -> Aff -> IO BasicSet


leBasicSet :: (Given Ctx) => Aff -> Aff -> BasicSet
leBasicSet = \aff1' aff2' -> trace "leBasicSet" $ 
    unsafePerformIO $ (return) =<< do
      aff1 <- (return) aff1'
      aff2 <- (return) aff2'

      let ctx = given :: Ctx
      c_leBasicSet aff1 aff2


foreign import ccall "isl_aff_lt_basic_set" c_ltBasicSet :: Aff -> Aff -> IO BasicSet


ltBasicSet :: (Given Ctx) => Aff -> Aff -> BasicSet
ltBasicSet = \aff1' aff2' -> trace "ltBasicSet" $ 
    unsafePerformIO $ (return) =<< do
      aff1 <- (return) aff1'
      aff2 <- (return) aff2'

      let ctx = given :: Ctx
      c_ltBasicSet aff1 aff2


foreign import ccall "isl_aff_neg_basic_set" c_negBasicSet :: Aff -> IO BasicSet


negBasicSet :: (Given Ctx) => Aff -> BasicSet
negBasicSet = \aff' -> trace "negBasicSet" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_negBasicSet aff


foreign import ccall "isl_aff_zero_basic_set" c_zeroBasicSet :: Aff -> IO BasicSet


zeroBasicSet :: (Given Ctx) => Aff -> BasicSet
zeroBasicSet = \aff' -> trace "zeroBasicSet" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_zeroBasicSet aff


foreign import ccall "isl_aff_get_coefficient_val" c_getCoefficientVal :: Aff -> DimType -> C.CInt -> IO Val


getCoefficientVal :: (Given Ctx) => Aff -> DimType -> Int -> Val
getCoefficientVal = \aff' typ' pos' -> trace "getCoefficientVal" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getCoefficientVal aff typ pos


foreign import ccall "isl_aff_get_denominator_val" c_getDenominatorVal :: Aff -> IO Val


getDenominatorVal :: (Given Ctx) => Aff -> Val
getDenominatorVal = \aff' -> trace "getDenominatorVal" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_getDenominatorVal aff


foreign import ccall "isl_aff_add_coefficient_si" c_addCoefficientSi :: Aff -> DimType -> C.CInt -> C.CInt -> IO Aff


addCoefficientSi :: (Given Ctx) => Aff -> DimType -> Int -> Int -> Aff
addCoefficientSi = \aff' typ' pos' v' -> trace "addCoefficientSi" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      v <- (return . fromIntegral) v'

      let ctx = given :: Ctx
      c_addCoefficientSi aff typ pos v


foreign import ccall "isl_aff_add_coefficient_val" c_addCoefficientVal :: Aff -> DimType -> C.CInt -> Val -> IO Aff


addCoefficientVal :: (Given Ctx) => Aff -> DimType -> Int -> Val -> Aff
addCoefficientVal = \aff' typ' pos' v' -> trace "addCoefficientVal" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      v <- (return) v'

      let ctx = given :: Ctx
      c_addCoefficientVal aff typ pos v


foreign import ccall "isl_aff_add_constant_num_si" c_addConstantNumSi :: Aff -> C.CInt -> IO Aff


addConstantNumSi :: (Given Ctx) => Aff -> Int -> Aff
addConstantNumSi = \aff' v' -> trace "addConstantNumSi" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      v <- (return . fromIntegral) v'

      let ctx = given :: Ctx
      c_addConstantNumSi aff v


foreign import ccall "isl_aff_add_constant_si" c_addConstantSi :: Aff -> C.CInt -> IO Aff


addConstantSi :: (Given Ctx) => Aff -> Int -> Aff
addConstantSi = \aff' v' -> trace "addConstantSi" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      v <- (return . fromIntegral) v'

      let ctx = given :: Ctx
      c_addConstantSi aff v


foreign import ccall "isl_aff_add_constant_val" c_addConstantVal :: Aff -> Val -> IO Aff


addConstantVal :: (Given Ctx) => Aff -> Val -> Aff
addConstantVal = \aff' v' -> trace "addConstantVal" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      v <- (return) v'

      let ctx = given :: Ctx
      c_addConstantVal aff v


foreign import ccall "isl_aff_add_dims" c_addDims :: Aff -> DimType -> C.CUInt -> IO Aff


addDims :: (Given Ctx) => Aff -> DimType -> Int -> Aff
addDims = \aff' typ' n' -> trace "addDims" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      typ <- (return) typ'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_addDims aff typ n


foreign import ccall "isl_aff_align_params" c_alignParams :: Aff -> Space -> IO Aff


alignParams :: (Given Ctx) => Aff -> Space -> Aff
alignParams = \aff' model' -> trace "alignParams" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      model <- (return) model'

      let ctx = given :: Ctx
      c_alignParams aff model


foreign import ccall "isl_aff_copy" c_copy :: Aff -> IO Aff


copy :: (Given Ctx) => Aff -> Aff
copy = \aff' -> trace "copy" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_copy aff


foreign import ccall "isl_aff_drop_dims" c_dropDims :: Aff -> DimType -> C.CUInt -> C.CUInt -> IO Aff


dropDims :: (Given Ctx) => Aff -> DimType -> Int -> Int -> Aff
dropDims = \aff' typ' first' n' -> trace "dropDims" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_dropDims aff typ first n


foreign import ccall "isl_aff_from_range" c_fromRange :: Aff -> IO Aff


fromRange :: (Given Ctx) => Aff -> Aff
fromRange = \aff' -> trace "fromRange" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_fromRange aff


foreign import ccall "isl_aff_get_div" c_getDiv :: Aff -> C.CInt -> IO Aff


getDiv :: (Given Ctx) => Aff -> Int -> Aff
getDiv = \aff' pos' -> trace "getDiv" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDiv aff pos


foreign import ccall "isl_aff_insert_dims" c_insertDims :: Aff -> DimType -> C.CUInt -> C.CUInt -> IO Aff


insertDims :: (Given Ctx) => Aff -> DimType -> Int -> Int -> Aff
insertDims = \aff' typ' first' n' -> trace "insertDims" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_insertDims aff typ first n


foreign import ccall "isl_aff_mod_val" c_modVal :: Aff -> Val -> IO Aff


modVal :: (Given Ctx) => Aff -> Val -> Aff
modVal = \aff' modulo' -> trace "modVal" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      modulo <- (return) modulo'

      let ctx = given :: Ctx
      c_modVal aff modulo


foreign import ccall "isl_aff_move_dims" c_moveDims :: Aff -> DimType -> C.CUInt -> DimType -> C.CUInt -> C.CUInt -> IO Aff


moveDims :: (Given Ctx) => Aff -> DimType -> Int -> DimType -> Int -> Int -> Aff
moveDims = \aff' dst_type' dst_pos' src_type' src_pos' n' -> trace "moveDims" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      dst_type <- (return) dst_type'
      dst_pos <- (return . fromIntegral) dst_pos'
      src_type <- (return) src_type'
      src_pos <- (return . fromIntegral) src_pos'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_moveDims aff dst_type dst_pos src_type src_pos n


foreign import ccall "isl_aff_nan_on_domain" c_nanOnDomain :: LocalSpace -> IO Aff


nanOnDomain :: (Given Ctx) => LocalSpace -> Aff
nanOnDomain = \ls' -> trace "nanOnDomain" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_nanOnDomain ls


foreign import ccall "isl_aff_nan_on_domain_space" c_nanOnDomainSpace :: Space -> IO Aff


nanOnDomainSpace :: (Given Ctx) => Space -> Aff
nanOnDomainSpace = \space' -> trace "nanOnDomainSpace" $ 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_nanOnDomainSpace space


foreign import ccall "isl_aff_param_on_domain_space_id" c_paramOnDomainSpaceId :: Space -> Id -> IO Aff


paramOnDomainSpaceId :: (Given Ctx) => Space -> Id -> Aff
paramOnDomainSpaceId = \space' id' -> trace "paramOnDomainSpaceId" $ 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      id <- (return) id'

      let ctx = given :: Ctx
      c_paramOnDomainSpaceId space id


foreign import ccall "isl_aff_project_domain_on_params" c_projectDomainOnParams :: Aff -> IO Aff


projectDomainOnParams :: (Given Ctx) => Aff -> Aff
projectDomainOnParams = \aff' -> trace "projectDomainOnParams" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_projectDomainOnParams aff


foreign import ccall "isl_aff_pullback_aff" c_pullbackAff :: Aff -> Aff -> IO Aff


pullbackAff :: (Given Ctx) => Aff -> Aff -> Aff
pullbackAff = \aff1' aff2' -> trace "pullbackAff" $ 
    unsafePerformIO $ (return) =<< do
      aff1 <- (return) aff1'
      aff2 <- (return) aff2'

      let ctx = given :: Ctx
      c_pullbackAff aff1 aff2


foreign import ccall "isl_aff_scale_down_ui" c_scaleDownUi :: Aff -> C.CUInt -> IO Aff


scaleDownUi :: (Given Ctx) => Aff -> Int -> Aff
scaleDownUi = \aff' f' -> trace "scaleDownUi" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      f <- (return . fromIntegral) f'

      let ctx = given :: Ctx
      c_scaleDownUi aff f


foreign import ccall "isl_aff_scale_down_val" c_scaleDownVal :: Aff -> Val -> IO Aff


scaleDownVal :: (Given Ctx) => Aff -> Val -> Aff
scaleDownVal = \aff' v' -> trace "scaleDownVal" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      v <- (return) v'

      let ctx = given :: Ctx
      c_scaleDownVal aff v


foreign import ccall "isl_aff_scale_val" c_scaleVal :: Aff -> Val -> IO Aff


scaleVal :: (Given Ctx) => Aff -> Val -> Aff
scaleVal = \aff' v' -> trace "scaleVal" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      v <- (return) v'

      let ctx = given :: Ctx
      c_scaleVal aff v


foreign import ccall "isl_aff_set_coefficient_si" c_setCoefficientSi :: Aff -> DimType -> C.CInt -> C.CInt -> IO Aff


setCoefficientSi :: (Given Ctx) => Aff -> DimType -> Int -> Int -> Aff
setCoefficientSi = \aff' typ' pos' v' -> trace "setCoefficientSi" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      v <- (return . fromIntegral) v'

      let ctx = given :: Ctx
      c_setCoefficientSi aff typ pos v


foreign import ccall "isl_aff_set_coefficient_val" c_setCoefficientVal :: Aff -> DimType -> C.CInt -> Val -> IO Aff


setCoefficientVal :: (Given Ctx) => Aff -> DimType -> Int -> Val -> Aff
setCoefficientVal = \aff' typ' pos' v' -> trace "setCoefficientVal" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      v <- (return) v'

      let ctx = given :: Ctx
      c_setCoefficientVal aff typ pos v


foreign import ccall "isl_aff_set_constant_si" c_setConstantSi :: Aff -> C.CInt -> IO Aff


setConstantSi :: (Given Ctx) => Aff -> Int -> Aff
setConstantSi = \aff' v' -> trace "setConstantSi" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      v <- (return . fromIntegral) v'

      let ctx = given :: Ctx
      c_setConstantSi aff v


foreign import ccall "isl_aff_set_constant_val" c_setConstantVal :: Aff -> Val -> IO Aff


setConstantVal :: (Given Ctx) => Aff -> Val -> Aff
setConstantVal = \aff' v' -> trace "setConstantVal" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      v <- (return) v'

      let ctx = given :: Ctx
      c_setConstantVal aff v


foreign import ccall "isl_aff_set_dim_id" c_setDimId :: Aff -> DimType -> C.CUInt -> Id -> IO Aff


setDimId :: (Given Ctx) => Aff -> DimType -> Int -> Id -> Aff
setDimId = \aff' typ' pos' id' -> trace "setDimId" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      id <- (return) id'

      let ctx = given :: Ctx
      c_setDimId aff typ pos id


foreign import ccall "isl_aff_set_dim_name" c_setDimName :: Aff -> DimType -> C.CUInt -> C.CString -> IO Aff


setDimName :: (Given Ctx) => Aff -> DimType -> Int -> String -> Aff
setDimName = \aff' typ' pos' s' -> trace "setDimName" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      s <- (C.newCString) s'

      let ctx = given :: Ctx
      c_setDimName aff typ pos s


foreign import ccall "isl_aff_set_tuple_id" c_setTupleId :: Aff -> DimType -> Id -> IO Aff


setTupleId :: (Given Ctx) => Aff -> DimType -> Id -> Aff
setTupleId = \aff' typ' id' -> trace "setTupleId" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      typ <- (return) typ'
      id <- (return) id'

      let ctx = given :: Ctx
      c_setTupleId aff typ id


foreign import ccall "isl_aff_val_on_domain" c_valOnDomain :: LocalSpace -> Val -> IO Aff


valOnDomain :: (Given Ctx) => LocalSpace -> Val -> Aff
valOnDomain = \ls' val' -> trace "valOnDomain" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'
      val <- (return) val'

      let ctx = given :: Ctx
      c_valOnDomain ls val


foreign import ccall "isl_aff_val_on_domain_space" c_valOnDomainSpace :: Space -> Val -> IO Aff


valOnDomainSpace :: (Given Ctx) => Space -> Val -> Aff
valOnDomainSpace = \space' val' -> trace "valOnDomainSpace" $ 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      val <- (return) val'

      let ctx = given :: Ctx
      c_valOnDomainSpace space val


foreign import ccall "isl_aff_var_on_domain" c_varOnDomain :: LocalSpace -> DimType -> C.CUInt -> IO Aff


varOnDomain :: (Given Ctx) => LocalSpace -> DimType -> Int -> Aff
varOnDomain = \ls' typ' pos' -> trace "varOnDomain" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_varOnDomain ls typ pos


foreign import ccall "isl_aff_zero_on_domain" c_zeroOnDomain :: LocalSpace -> IO Aff


zeroOnDomain :: (Given Ctx) => LocalSpace -> Aff
zeroOnDomain = \ls' -> trace "zeroOnDomain" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_zeroOnDomain ls


foreign import ccall "isl_aff_zero_on_domain_space" c_zeroOnDomainSpace :: Space -> IO Aff


zeroOnDomainSpace :: (Given Ctx) => Space -> Aff
zeroOnDomainSpace = \space' -> trace "zeroOnDomainSpace" $ 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_zeroOnDomainSpace space


foreign import ccall "isl_aff_to_str" c_toStr :: Aff -> IO C.CString


toStr :: (Given Ctx) => Aff -> String
toStr = \aff' -> trace "toStr" $ 
    unsafePerformIO $ (C.peekCString) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_toStr aff


foreign import ccall "isl_aff_get_domain_local_space" c_getDomainLocalSpace :: Aff -> IO LocalSpace


getDomainLocalSpace :: (Given Ctx) => Aff -> LocalSpace
getDomainLocalSpace = \aff' -> trace "getDomainLocalSpace" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_getDomainLocalSpace aff


foreign import ccall "isl_aff_get_local_space" c_getLocalSpace :: Aff -> IO LocalSpace


getLocalSpace :: (Given Ctx) => Aff -> LocalSpace
getLocalSpace = \aff' -> trace "getLocalSpace" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_getLocalSpace aff


foreign import ccall "isl_aff_is_cst" c_isCst :: Aff -> IO C.CBool


isCst :: (Given Ctx) => Aff -> Bool
isCst = \aff' -> trace "isCst" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_isCst aff


foreign import ccall "isl_aff_plain_is_equal" c_plainIsEqual :: Aff -> Aff -> IO C.CBool


plainIsEqual :: (Given Ctx) => Aff -> Aff -> Bool
plainIsEqual = \aff1' aff2' -> trace "plainIsEqual" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      aff1 <- (return) aff1'
      aff2 <- (return) aff2'

      let ctx = given :: Ctx
      c_plainIsEqual aff1 aff2


foreign import ccall "isl_aff_eq_set" c_eqSet :: Aff -> Aff -> IO Set


eqSet :: (Given Ctx) => Aff -> Aff -> Set
eqSet = \aff1' aff2' -> trace "eqSet" $ 
    unsafePerformIO $ (return) =<< do
      aff1 <- (return) aff1'
      aff2 <- (return) aff2'

      let ctx = given :: Ctx
      c_eqSet aff1 aff2


foreign import ccall "isl_aff_ge_set" c_geSet :: Aff -> Aff -> IO Set


geSet :: (Given Ctx) => Aff -> Aff -> Set
geSet = \aff1' aff2' -> trace "geSet" $ 
    unsafePerformIO $ (return) =<< do
      aff1 <- (return) aff1'
      aff2 <- (return) aff2'

      let ctx = given :: Ctx
      c_geSet aff1 aff2


foreign import ccall "isl_aff_gt_set" c_gtSet :: Aff -> Aff -> IO Set


gtSet :: (Given Ctx) => Aff -> Aff -> Set
gtSet = \aff1' aff2' -> trace "gtSet" $ 
    unsafePerformIO $ (return) =<< do
      aff1 <- (return) aff1'
      aff2 <- (return) aff2'

      let ctx = given :: Ctx
      c_gtSet aff1 aff2


foreign import ccall "isl_aff_le_set" c_leSet :: Aff -> Aff -> IO Set


leSet :: (Given Ctx) => Aff -> Aff -> Set
leSet = \aff1' aff2' -> trace "leSet" $ 
    unsafePerformIO $ (return) =<< do
      aff1 <- (return) aff1'
      aff2 <- (return) aff2'

      let ctx = given :: Ctx
      c_leSet aff1 aff2


foreign import ccall "isl_aff_lt_set" c_ltSet :: Aff -> Aff -> IO Set


ltSet :: (Given Ctx) => Aff -> Aff -> Set
ltSet = \aff1' aff2' -> trace "ltSet" $ 
    unsafePerformIO $ (return) =<< do
      aff1 <- (return) aff1'
      aff2 <- (return) aff2'

      let ctx = given :: Ctx
      c_ltSet aff1 aff2


foreign import ccall "isl_aff_ne_set" c_neSet :: Aff -> Aff -> IO Set


neSet :: (Given Ctx) => Aff -> Aff -> Set
neSet = \aff1' aff2' -> trace "neSet" $ 
    unsafePerformIO $ (return) =<< do
      aff1 <- (return) aff1'
      aff2 <- (return) aff2'

      let ctx = given :: Ctx
      c_neSet aff1 aff2


foreign import ccall "isl_aff_get_constant_val" c_getConstantVal :: Aff -> IO Val


getConstantVal :: (Given Ctx) => Aff -> Val
getConstantVal = \aff' -> trace "getConstantVal" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_getConstantVal aff


foreign import ccall "isl_aff_add" c_add :: Aff -> Aff -> IO Aff


add :: (Given Ctx) => Aff -> Aff -> Aff
add = \aff1' aff2' -> trace "add" $ 
    unsafePerformIO $ (return) =<< do
      aff1 <- (return) aff1'
      aff2 <- (return) aff2'

      let ctx = given :: Ctx
      c_add aff1 aff2


foreign import ccall "isl_aff_ceil" c_ceil :: Aff -> IO Aff


ceil :: (Given Ctx) => Aff -> Aff
ceil = \aff' -> trace "ceil" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_ceil aff


foreign import ccall "isl_aff_div" c_div :: Aff -> Aff -> IO Aff


div :: (Given Ctx) => Aff -> Aff -> Aff
div = \aff1' aff2' -> trace "div" $ 
    unsafePerformIO $ (return) =<< do
      aff1 <- (return) aff1'
      aff2 <- (return) aff2'

      let ctx = given :: Ctx
      c_div aff1 aff2


foreign import ccall "isl_aff_domain_reverse" c_domainReverse :: Aff -> IO Aff


domainReverse :: (Given Ctx) => Aff -> Aff
domainReverse = \aff' -> trace "domainReverse" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_domainReverse aff


foreign import ccall "isl_aff_floor" c_floor :: Aff -> IO Aff


floor :: (Given Ctx) => Aff -> Aff
floor = \aff' -> trace "floor" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_floor aff


foreign import ccall "isl_aff_gist" c_gist :: Aff -> Set -> IO Aff


gist :: (Given Ctx) => Aff -> Set -> Aff
gist = \aff' context' -> trace "gist" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      context <- (return) context'

      let ctx = given :: Ctx
      c_gist aff context


foreign import ccall "isl_aff_gist_params" c_gistParams :: Aff -> Set -> IO Aff


gistParams :: (Given Ctx) => Aff -> Set -> Aff
gistParams = \aff' context' -> trace "gistParams" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'
      context <- (return) context'

      let ctx = given :: Ctx
      c_gistParams aff context


foreign import ccall "isl_aff_mul" c_mul :: Aff -> Aff -> IO Aff


mul :: (Given Ctx) => Aff -> Aff -> Aff
mul = \aff1' aff2' -> trace "mul" $ 
    unsafePerformIO $ (return) =<< do
      aff1 <- (return) aff1'
      aff2 <- (return) aff2'

      let ctx = given :: Ctx
      c_mul aff1 aff2


foreign import ccall "isl_aff_neg" c_neg :: Aff -> IO Aff


neg :: (Given Ctx) => Aff -> Aff
neg = \aff' -> trace "neg" $ 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_neg aff


foreign import ccall "isl_aff_sub" c_sub :: Aff -> Aff -> IO Aff


sub :: (Given Ctx) => Aff -> Aff -> Aff
sub = \aff1' aff2' -> trace "sub" $ 
    unsafePerformIO $ (return) =<< do
      aff1 <- (return) aff1'
      aff2 <- (return) aff2'

      let ctx = given :: Ctx
      c_sub aff1 aff2


foreign import ccall "isl_aff_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO Aff


readFromStr :: (Given Ctx) => String -> Aff
readFromStr = \str' -> trace "readFromStr" $ 
    unsafePerformIO $ (return) =<< do
      str <- (C.newCString) str'

      let ctx = given :: Ctx
      c_readFromStr ctx str


