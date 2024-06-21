{-# OPTIONS_GHC -Wno-all #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE TypeData #-}
module GHC.Wasm.Web.Generated.DOMMatrix (
        DOMMatrix, DOMMatrixClass, js_cons_DOMMatrix_DOMString,
        js_cons_DOMMatrix_DOMMatrixReadOnly,
        js_cons_DOMMatrix_Float32Array, js_cons_DOMMatrix_Float64Array,
        js_cons_DOMMatrix_sequence_double,
        js_fun_multiplySelf_DOMMatrix_DOMMatrix,
        js_fun_preMultiplySelf_DOMMatrix_DOMMatrix,
        js_fun_translateSelf_double_double_nullable_double_DOMMatrix,
        js_fun_scaleSelf_double_nullable_double_nullable_double_DOMMatrix,
        js_fun_scale3dSelf_double_nullable_double_nullable_double_nullable_double_DOMMatrix,
        js_fun_scaleNonUniformSelf_double_nullable_double_nullable_double_nullable_double_nullable_double_nullable_double_DOMMatrix,
        js_fun_rotateSelf_double_nullable_double_nullable_double_DOMMatrix,
        js_fun_rotateFromVectorSelf_double_double_DOMMatrix,
        js_fun_rotateAxisAngleSelf_double_double_double_double_DOMMatrix,
        js_fun_skewXSelf_double_DOMMatrix,
        js_fun_skewYSelf_double_DOMMatrix, js_fun_invertSelf__DOMMatrix,
        js_fun_setMatrixValue_DOMString_DOMMatrix, js_get_a, js_set_a,
        js_get_b, js_set_b, js_get_c, js_set_c, js_get_d, js_set_d,
        js_get_e, js_set_e, js_get_f, js_set_f, js_get_m11, js_set_m11,
        js_get_m12, js_set_m12, js_get_m13, js_set_m13, js_get_m14,
        js_set_m14, js_get_m21, js_set_m21, js_get_m22, js_set_m22,
        js_get_m23, js_set_m23, js_get_m24, js_set_m24, js_get_m31,
        js_set_m31, js_get_m32, js_set_m32, js_get_m33, js_set_m33,
        js_get_m34, js_set_m34, js_get_m41, js_set_m41, js_get_m42,
        js_set_m42, js_get_m43, js_set_m43, js_get_m44, js_set_m44
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMMatrix.Core
import GHC.Wasm.Web.Generated.DOMMatrixReadOnly.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new DOMMatrix($1)" js_cons_DOMMatrix_DOMString
  :: DOMString -> (IO DOMMatrix)
foreign import javascript unsafe "new DOMMatrix($1)" js_cons_DOMMatrix_DOMMatrixReadOnly
  :: DOMMatrixReadOnly -> (IO DOMMatrix)
foreign import javascript unsafe "new DOMMatrix($1)" js_cons_DOMMatrix_Float32Array
  :: JSByteArray Float -> (IO DOMMatrix)
foreign import javascript unsafe "new DOMMatrix($1)" js_cons_DOMMatrix_Float64Array
  :: JSByteArray Double -> (IO DOMMatrix)
foreign import javascript unsafe "new DOMMatrix($1)" js_cons_DOMMatrix_sequence_double
  :: Sequence (JSPrimClass Double) -> (IO DOMMatrix)
foreign import javascript unsafe "$1.multiplySelf($2)" js_fun_multiplySelf_DOMMatrix_DOMMatrix
  :: DOMMatrix -> (DOMMatrix -> (IO DOMMatrix))
foreign import javascript unsafe "$1.preMultiplySelf($2)" js_fun_preMultiplySelf_DOMMatrix_DOMMatrix
  :: DOMMatrix -> (DOMMatrix -> (IO DOMMatrix))
foreign import javascript unsafe "$1.translateSelf($2,$3,$4)" js_fun_translateSelf_double_double_nullable_double_DOMMatrix
  :: DOMMatrix
     -> (Double
         -> (Double -> (Nullable (JSPrimClass Double) -> (IO DOMMatrix))))
foreign import javascript unsafe "$1.scaleSelf($2,$3,$4)" js_fun_scaleSelf_double_nullable_double_nullable_double_DOMMatrix
  :: DOMMatrix
     -> (Double
         -> (Nullable (JSPrimClass Double)
             -> (Nullable (JSPrimClass Double) -> (IO DOMMatrix))))
foreign import javascript unsafe "$1.scale3dSelf($2,$3,$4,$5)" js_fun_scale3dSelf_double_nullable_double_nullable_double_nullable_double_DOMMatrix
  :: DOMMatrix
     -> (Double
         -> (Nullable (JSPrimClass Double)
             -> (Nullable (JSPrimClass Double)
                 -> (Nullable (JSPrimClass Double) -> (IO DOMMatrix)))))
foreign import javascript unsafe "$1.scaleNonUniformSelf($2,$3,$4,$5,$6,$7)" js_fun_scaleNonUniformSelf_double_nullable_double_nullable_double_nullable_double_nullable_double_nullable_double_DOMMatrix
  :: DOMMatrix
     -> (Double
         -> (Nullable (JSPrimClass Double)
             -> (Nullable (JSPrimClass Double)
                 -> (Nullable (JSPrimClass Double)
                     -> (Nullable (JSPrimClass Double)
                         -> (Nullable (JSPrimClass Double) -> (IO DOMMatrix)))))))
foreign import javascript unsafe "$1.rotateSelf($2,$3,$4)" js_fun_rotateSelf_double_nullable_double_nullable_double_DOMMatrix
  :: DOMMatrix
     -> (Double
         -> (Nullable (JSPrimClass Double)
             -> (Nullable (JSPrimClass Double) -> (IO DOMMatrix))))
foreign import javascript unsafe "$1.rotateFromVectorSelf($2,$3)" js_fun_rotateFromVectorSelf_double_double_DOMMatrix
  :: DOMMatrix -> (Double -> (Double -> (IO DOMMatrix)))
foreign import javascript unsafe "$1.rotateAxisAngleSelf($2,$3,$4,$5)" js_fun_rotateAxisAngleSelf_double_double_double_double_DOMMatrix
  :: DOMMatrix
     -> (Double -> (Double -> (Double -> (Double -> (IO DOMMatrix)))))
foreign import javascript unsafe "$1.skewXSelf($2)" js_fun_skewXSelf_double_DOMMatrix
  :: DOMMatrix -> (Double -> (IO DOMMatrix))
foreign import javascript unsafe "$1.skewYSelf($2)" js_fun_skewYSelf_double_DOMMatrix
  :: DOMMatrix -> (Double -> (IO DOMMatrix))
foreign import javascript unsafe "$1.invertSelf()" js_fun_invertSelf__DOMMatrix
  :: DOMMatrix -> (IO DOMMatrix)
foreign import javascript unsafe "$1.setMatrixValue($2)" js_fun_setMatrixValue_DOMString_DOMMatrix
  :: DOMMatrix -> (DOMString -> (IO DOMMatrix))
foreign import javascript unsafe "$1.a" js_get_a
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.a = $2" js_set_a
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.b" js_get_b
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.b = $2" js_set_b
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.c" js_get_c
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.c = $2" js_set_c
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.d" js_get_d
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.d = $2" js_set_d
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.e" js_get_e
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.e = $2" js_set_e
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.f" js_get_f
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.f = $2" js_set_f
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.m11" js_get_m11
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.m11 = $2" js_set_m11
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.m12" js_get_m12
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.m12 = $2" js_set_m12
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.m13" js_get_m13
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.m13 = $2" js_set_m13
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.m14" js_get_m14
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.m14 = $2" js_set_m14
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.m21" js_get_m21
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.m21 = $2" js_set_m21
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.m22" js_get_m22
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.m22 = $2" js_set_m22
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.m23" js_get_m23
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.m23 = $2" js_set_m23
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.m24" js_get_m24
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.m24 = $2" js_set_m24
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.m31" js_get_m31
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.m31 = $2" js_set_m31
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.m32" js_get_m32
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.m32 = $2" js_set_m32
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.m33" js_get_m33
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.m33 = $2" js_set_m33
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.m34" js_get_m34
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.m34 = $2" js_set_m34
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.m41" js_get_m41
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.m41 = $2" js_set_m41
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.m42" js_get_m42
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.m42 = $2" js_set_m42
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.m43" js_get_m43
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.m43 = $2" js_set_m43
  :: DOMMatrix -> (Double -> (IO ()))
foreign import javascript unsafe "$1.m44" js_get_m44
  :: DOMMatrix -> (IO Double)
foreign import javascript unsafe "$1.m44 = $2" js_set_m44
  :: DOMMatrix -> (Double -> (IO ()))
