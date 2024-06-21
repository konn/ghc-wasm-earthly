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
module GHC.Wasm.Web.Generated.SVGAngle (
        SVGAngle, SVGAngleClass, js_const_SVGAngle_SVG_ANGLETYPE_UNKNOWN,
        js_const_SVGAngle_SVG_ANGLETYPE_UNSPECIFIED,
        js_const_SVGAngle_SVG_ANGLETYPE_DEG,
        js_const_SVGAngle_SVG_ANGLETYPE_RAD,
        js_const_SVGAngle_SVG_ANGLETYPE_GRAD,
        js_fun_newValueSpecifiedUnits_short_float_undefined,
        js_fun_convertToSpecifiedUnits_short_undefined, js_get_unitType,
        js_get_value, js_set_value, js_get_valueInSpecifiedUnits,
        js_set_valueInSpecifiedUnits, js_get_valueAsString,
        js_set_valueAsString
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.SVGAngle.Core
import GHC.Wasm.Web.Types
js_const_SVGAngle_SVG_ANGLETYPE_UNKNOWN :: Word16
js_const_SVGAngle_SVG_ANGLETYPE_UNKNOWN = 0
js_const_SVGAngle_SVG_ANGLETYPE_UNSPECIFIED :: Word16
js_const_SVGAngle_SVG_ANGLETYPE_UNSPECIFIED = 1
js_const_SVGAngle_SVG_ANGLETYPE_DEG :: Word16
js_const_SVGAngle_SVG_ANGLETYPE_DEG = 2
js_const_SVGAngle_SVG_ANGLETYPE_RAD :: Word16
js_const_SVGAngle_SVG_ANGLETYPE_RAD = 3
js_const_SVGAngle_SVG_ANGLETYPE_GRAD :: Word16
js_const_SVGAngle_SVG_ANGLETYPE_GRAD = 4
foreign import javascript unsafe "$1.newValueSpecifiedUnits($2,$3)" js_fun_newValueSpecifiedUnits_short_float_undefined
  :: SVGAngle -> (Word16 -> (Float -> (IO ())))
foreign import javascript unsafe "$1.convertToSpecifiedUnits($2)" js_fun_convertToSpecifiedUnits_short_undefined
  :: SVGAngle -> (Word16 -> (IO ()))
foreign import javascript unsafe "$1.unitType" js_get_unitType
  :: SVGAngle -> (IO Word16)
foreign import javascript unsafe "$1.value" js_get_value
  :: SVGAngle -> (IO Float)
foreign import javascript unsafe "$1.value = $2" js_set_value
  :: SVGAngle -> (Float -> (IO ()))
foreign import javascript unsafe "$1.valueInSpecifiedUnits" js_get_valueInSpecifiedUnits
  :: SVGAngle -> (IO Float)
foreign import javascript unsafe "$1.valueInSpecifiedUnits = $2" js_set_valueInSpecifiedUnits
  :: SVGAngle -> (Float -> (IO ()))
foreign import javascript unsafe "$1.valueAsString" js_get_valueAsString
  :: SVGAngle -> (IO DOMString)
foreign import javascript unsafe "$1.valueAsString = $2" js_set_valueAsString
  :: SVGAngle -> (DOMString -> (IO ()))
