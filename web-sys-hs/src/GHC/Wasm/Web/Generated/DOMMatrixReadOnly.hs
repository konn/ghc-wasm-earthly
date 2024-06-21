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
module GHC.Wasm.Web.Generated.DOMMatrixReadOnly (
        DOMMatrixReadOnly, DOMMatrixReadOnlyClass,
        js_cons_DOMMatrixReadOnly,
        js_fun_translate_double_double_nullable_double_DOMMatrix,
        js_fun_scale_double_nullable_double_nullable_double_DOMMatrix,
        js_fun_scale3d_double_nullable_double_nullable_double_nullable_double_DOMMatrix,
        js_fun_scaleNonUniform_double_nullable_double_nullable_double_nullable_double_nullable_double_nullable_double_DOMMatrix,
        js_fun_rotate_double_nullable_double_nullable_double_DOMMatrix,
        js_fun_rotateFromVector_double_double_DOMMatrix,
        js_fun_rotateAxisAngle_double_double_double_double_DOMMatrix,
        js_fun_skewX_double_DOMMatrix, js_fun_skewY_double_DOMMatrix,
        js_fun_multiply_DOMMatrix_DOMMatrix, js_fun_flipX__DOMMatrix,
        js_fun_flipY__DOMMatrix, js_fun_inverse__DOMMatrix,
        js_fun_transformPoint_nullable_DOMPointInit_DOMPoint,
        js_fun_toFloat32Array__Float32Array,
        js_fun_toFloat64Array__Float64Array, js_fun_toJSON__object,
        js_get_a, js_get_b, js_get_c, js_get_d, js_get_e, js_get_f,
        js_get_m11, js_get_m12, js_get_m13, js_get_m14, js_get_m21,
        js_get_m22, js_get_m23, js_get_m24, js_get_m31, js_get_m32,
        js_get_m33, js_get_m34, js_get_m41, js_get_m42, js_get_m43,
        js_get_m44, js_get_is2D, js_get_isIdentity
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMMatrix.Core
import GHC.Wasm.Web.Generated.DOMMatrixReadOnly.Core
import GHC.Wasm.Web.Generated.DOMPoint.Core
import GHC.Wasm.Web.Generated.DOMPointInit.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new DOMMatrixReadOnly($1)" js_cons_DOMMatrixReadOnly
  :: Nullable (UnionClass '[DOMStringClass,
                            SequenceClass (JSPrimClass Double)])
     -> (IO DOMMatrixReadOnly)
foreign import javascript unsafe "$1.translate($2,$3,$4)" js_fun_translate_double_double_nullable_double_DOMMatrix
  :: DOMMatrixReadOnly
     -> (Double
         -> (Double -> (Nullable (JSPrimClass Double) -> (IO DOMMatrix))))
foreign import javascript unsafe "$1.scale($2,$3,$4)" js_fun_scale_double_nullable_double_nullable_double_DOMMatrix
  :: DOMMatrixReadOnly
     -> (Double
         -> (Nullable (JSPrimClass Double)
             -> (Nullable (JSPrimClass Double) -> (IO DOMMatrix))))
foreign import javascript unsafe "$1.scale3d($2,$3,$4,$5)" js_fun_scale3d_double_nullable_double_nullable_double_nullable_double_DOMMatrix
  :: DOMMatrixReadOnly
     -> (Double
         -> (Nullable (JSPrimClass Double)
             -> (Nullable (JSPrimClass Double)
                 -> (Nullable (JSPrimClass Double) -> (IO DOMMatrix)))))
foreign import javascript unsafe "$1.scaleNonUniform($2,$3,$4,$5,$6,$7)" js_fun_scaleNonUniform_double_nullable_double_nullable_double_nullable_double_nullable_double_nullable_double_DOMMatrix
  :: DOMMatrixReadOnly
     -> (Double
         -> (Nullable (JSPrimClass Double)
             -> (Nullable (JSPrimClass Double)
                 -> (Nullable (JSPrimClass Double)
                     -> (Nullable (JSPrimClass Double)
                         -> (Nullable (JSPrimClass Double) -> (IO DOMMatrix)))))))
foreign import javascript unsafe "$1.rotate($2,$3,$4)" js_fun_rotate_double_nullable_double_nullable_double_DOMMatrix
  :: DOMMatrixReadOnly
     -> (Double
         -> (Nullable (JSPrimClass Double)
             -> (Nullable (JSPrimClass Double) -> (IO DOMMatrix))))
foreign import javascript unsafe "$1.rotateFromVector($2,$3)" js_fun_rotateFromVector_double_double_DOMMatrix
  :: DOMMatrixReadOnly -> (Double -> (Double -> (IO DOMMatrix)))
foreign import javascript unsafe "$1.rotateAxisAngle($2,$3,$4,$5)" js_fun_rotateAxisAngle_double_double_double_double_DOMMatrix
  :: DOMMatrixReadOnly
     -> (Double -> (Double -> (Double -> (Double -> (IO DOMMatrix)))))
foreign import javascript unsafe "$1.skewX($2)" js_fun_skewX_double_DOMMatrix
  :: DOMMatrixReadOnly -> (Double -> (IO DOMMatrix))
foreign import javascript unsafe "$1.skewY($2)" js_fun_skewY_double_DOMMatrix
  :: DOMMatrixReadOnly -> (Double -> (IO DOMMatrix))
foreign import javascript unsafe "$1.multiply($2)" js_fun_multiply_DOMMatrix_DOMMatrix
  :: DOMMatrixReadOnly -> (DOMMatrix -> (IO DOMMatrix))
foreign import javascript unsafe "$1.flipX()" js_fun_flipX__DOMMatrix
  :: DOMMatrixReadOnly -> (IO DOMMatrix)
foreign import javascript unsafe "$1.flipY()" js_fun_flipY__DOMMatrix
  :: DOMMatrixReadOnly -> (IO DOMMatrix)
foreign import javascript unsafe "$1.inverse()" js_fun_inverse__DOMMatrix
  :: DOMMatrixReadOnly -> (IO DOMMatrix)
foreign import javascript unsafe "$1.transformPoint($2)" js_fun_transformPoint_nullable_DOMPointInit_DOMPoint
  :: DOMMatrixReadOnly
     -> (Nullable DOMPointInitClass -> (IO DOMPoint))
foreign import javascript unsafe "$1.toFloat32Array()" js_fun_toFloat32Array__Float32Array
  :: DOMMatrixReadOnly -> (IO (JSByteArray Float))
foreign import javascript unsafe "$1.toFloat64Array()" js_fun_toFloat64Array__Float64Array
  :: DOMMatrixReadOnly -> (IO (JSByteArray Double))
foreign import javascript unsafe "$1.toJSON()" js_fun_toJSON__object
  :: DOMMatrixReadOnly -> (IO JSAny)
foreign import javascript unsafe "$1.a" js_get_a
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.b" js_get_b
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.c" js_get_c
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.d" js_get_d
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.e" js_get_e
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.f" js_get_f
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.m11" js_get_m11
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.m12" js_get_m12
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.m13" js_get_m13
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.m14" js_get_m14
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.m21" js_get_m21
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.m22" js_get_m22
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.m23" js_get_m23
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.m24" js_get_m24
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.m31" js_get_m31
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.m32" js_get_m32
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.m33" js_get_m33
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.m34" js_get_m34
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.m41" js_get_m41
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.m42" js_get_m42
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.m43" js_get_m43
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.m44" js_get_m44
  :: DOMMatrixReadOnly -> (IO Double)
foreign import javascript unsafe "$1.is2D" js_get_is2D
  :: DOMMatrixReadOnly -> (IO Bool)
foreign import javascript unsafe "$1.isIdentity" js_get_isIdentity
  :: DOMMatrixReadOnly -> (IO Bool)
