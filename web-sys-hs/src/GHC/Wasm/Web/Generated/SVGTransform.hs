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
module GHC.Wasm.Web.Generated.SVGTransform (
        SVGTransform, SVGTransformClass,
        js_const_SVGTransform_SVG_TRANSFORM_UNKNOWN,
        js_const_SVGTransform_SVG_TRANSFORM_MATRIX,
        js_const_SVGTransform_SVG_TRANSFORM_TRANSLATE,
        js_const_SVGTransform_SVG_TRANSFORM_SCALE,
        js_const_SVGTransform_SVG_TRANSFORM_ROTATE,
        js_const_SVGTransform_SVG_TRANSFORM_SKEWX,
        js_const_SVGTransform_SVG_TRANSFORM_SKEWY,
        js_fun_setMatrix_SVGMatrix_undefined,
        js_fun_setTranslate_float_float_undefined,
        js_fun_setScale_float_float_undefined,
        js_fun_setRotate_float_float_float_undefined,
        js_fun_setSkewX_float_undefined, js_fun_setSkewY_float_undefined,
        js_get_type, js_get_matrix, js_get_angle
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.SVGMatrix.Core
import GHC.Wasm.Web.Generated.SVGTransform.Core
import GHC.Wasm.Web.Types
js_const_SVGTransform_SVG_TRANSFORM_UNKNOWN :: Word16
js_const_SVGTransform_SVG_TRANSFORM_UNKNOWN = 0
js_const_SVGTransform_SVG_TRANSFORM_MATRIX :: Word16
js_const_SVGTransform_SVG_TRANSFORM_MATRIX = 1
js_const_SVGTransform_SVG_TRANSFORM_TRANSLATE :: Word16
js_const_SVGTransform_SVG_TRANSFORM_TRANSLATE = 2
js_const_SVGTransform_SVG_TRANSFORM_SCALE :: Word16
js_const_SVGTransform_SVG_TRANSFORM_SCALE = 3
js_const_SVGTransform_SVG_TRANSFORM_ROTATE :: Word16
js_const_SVGTransform_SVG_TRANSFORM_ROTATE = 4
js_const_SVGTransform_SVG_TRANSFORM_SKEWX :: Word16
js_const_SVGTransform_SVG_TRANSFORM_SKEWX = 5
js_const_SVGTransform_SVG_TRANSFORM_SKEWY :: Word16
js_const_SVGTransform_SVG_TRANSFORM_SKEWY = 6
foreign import javascript unsafe "$1.setMatrix($2)" js_fun_setMatrix_SVGMatrix_undefined
  :: SVGTransform -> (SVGMatrix -> (IO ()))
foreign import javascript unsafe "$1.setTranslate($2,$3)" js_fun_setTranslate_float_float_undefined
  :: SVGTransform -> (Float -> (Float -> (IO ())))
foreign import javascript unsafe "$1.setScale($2,$3)" js_fun_setScale_float_float_undefined
  :: SVGTransform -> (Float -> (Float -> (IO ())))
foreign import javascript unsafe "$1.setRotate($2,$3,$4)" js_fun_setRotate_float_float_float_undefined
  :: SVGTransform -> (Float -> (Float -> (Float -> (IO ()))))
foreign import javascript unsafe "$1.setSkewX($2)" js_fun_setSkewX_float_undefined
  :: SVGTransform -> (Float -> (IO ()))
foreign import javascript unsafe "$1.setSkewY($2)" js_fun_setSkewY_float_undefined
  :: SVGTransform -> (Float -> (IO ()))
foreign import javascript unsafe "$1.type" js_get_type
  :: SVGTransform -> (IO Word16)
foreign import javascript unsafe "$1.matrix" js_get_matrix
  :: SVGTransform -> (IO SVGMatrix)
foreign import javascript unsafe "$1.angle" js_get_angle
  :: SVGTransform -> (IO Float)
