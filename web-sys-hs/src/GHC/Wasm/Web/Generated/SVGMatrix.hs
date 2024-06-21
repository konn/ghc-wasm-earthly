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
module GHC.Wasm.Web.Generated.SVGMatrix (
        SVGMatrix, SVGMatrixClass, js_fun_multiply_SVGMatrix_SVGMatrix,
        js_fun_inverse__SVGMatrix, js_fun_translate_float_float_SVGMatrix,
        js_fun_scale_float_SVGMatrix,
        js_fun_scaleNonUniform_float_float_SVGMatrix,
        js_fun_rotate_float_SVGMatrix,
        js_fun_rotateFromVector_float_float_SVGMatrix,
        js_fun_flipX__SVGMatrix, js_fun_flipY__SVGMatrix,
        js_fun_skewX_float_SVGMatrix, js_fun_skewY_float_SVGMatrix,
        js_get_a, js_set_a, js_get_b, js_set_b, js_get_c, js_set_c,
        js_get_d, js_set_d, js_get_e, js_set_e, js_get_f, js_set_f
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.SVGMatrix.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.multiply($2)" js_fun_multiply_SVGMatrix_SVGMatrix
  :: SVGMatrix -> (SVGMatrix -> (IO SVGMatrix))
foreign import javascript unsafe "$1.inverse()" js_fun_inverse__SVGMatrix
  :: SVGMatrix -> (IO SVGMatrix)
foreign import javascript unsafe "$1.translate($2,$3)" js_fun_translate_float_float_SVGMatrix
  :: SVGMatrix -> (Float -> (Float -> (IO SVGMatrix)))
foreign import javascript unsafe "$1.scale($2)" js_fun_scale_float_SVGMatrix
  :: SVGMatrix -> (Float -> (IO SVGMatrix))
foreign import javascript unsafe "$1.scaleNonUniform($2,$3)" js_fun_scaleNonUniform_float_float_SVGMatrix
  :: SVGMatrix -> (Float -> (Float -> (IO SVGMatrix)))
foreign import javascript unsafe "$1.rotate($2)" js_fun_rotate_float_SVGMatrix
  :: SVGMatrix -> (Float -> (IO SVGMatrix))
foreign import javascript unsafe "$1.rotateFromVector($2,$3)" js_fun_rotateFromVector_float_float_SVGMatrix
  :: SVGMatrix -> (Float -> (Float -> (IO SVGMatrix)))
foreign import javascript unsafe "$1.flipX()" js_fun_flipX__SVGMatrix
  :: SVGMatrix -> (IO SVGMatrix)
foreign import javascript unsafe "$1.flipY()" js_fun_flipY__SVGMatrix
  :: SVGMatrix -> (IO SVGMatrix)
foreign import javascript unsafe "$1.skewX($2)" js_fun_skewX_float_SVGMatrix
  :: SVGMatrix -> (Float -> (IO SVGMatrix))
foreign import javascript unsafe "$1.skewY($2)" js_fun_skewY_float_SVGMatrix
  :: SVGMatrix -> (Float -> (IO SVGMatrix))
foreign import javascript unsafe "$1.a" js_get_a
  :: SVGMatrix -> (IO Float)
foreign import javascript unsafe "$1.a = $2" js_set_a
  :: SVGMatrix -> (Float -> (IO ()))
foreign import javascript unsafe "$1.b" js_get_b
  :: SVGMatrix -> (IO Float)
foreign import javascript unsafe "$1.b = $2" js_set_b
  :: SVGMatrix -> (Float -> (IO ()))
foreign import javascript unsafe "$1.c" js_get_c
  :: SVGMatrix -> (IO Float)
foreign import javascript unsafe "$1.c = $2" js_set_c
  :: SVGMatrix -> (Float -> (IO ()))
foreign import javascript unsafe "$1.d" js_get_d
  :: SVGMatrix -> (IO Float)
foreign import javascript unsafe "$1.d = $2" js_set_d
  :: SVGMatrix -> (Float -> (IO ()))
foreign import javascript unsafe "$1.e" js_get_e
  :: SVGMatrix -> (IO Float)
foreign import javascript unsafe "$1.e = $2" js_set_e
  :: SVGMatrix -> (Float -> (IO ()))
foreign import javascript unsafe "$1.f" js_get_f
  :: SVGMatrix -> (IO Float)
foreign import javascript unsafe "$1.f = $2" js_set_f
  :: SVGMatrix -> (Float -> (IO ()))
