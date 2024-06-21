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
module GHC.Wasm.Web.Generated.SVGLength (
        SVGLength, SVGLengthClass,
        js_const_SVGLength_SVG_LENGTHTYPE_UNKNOWN,
        js_const_SVGLength_SVG_LENGTHTYPE_NUMBER,
        js_const_SVGLength_SVG_LENGTHTYPE_PERCENTAGE,
        js_const_SVGLength_SVG_LENGTHTYPE_EMS,
        js_const_SVGLength_SVG_LENGTHTYPE_EXS,
        js_const_SVGLength_SVG_LENGTHTYPE_PX,
        js_const_SVGLength_SVG_LENGTHTYPE_CM,
        js_const_SVGLength_SVG_LENGTHTYPE_MM,
        js_const_SVGLength_SVG_LENGTHTYPE_IN,
        js_const_SVGLength_SVG_LENGTHTYPE_PT,
        js_const_SVGLength_SVG_LENGTHTYPE_PC,
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
import GHC.Wasm.Web.Generated.SVGLength.Core
import GHC.Wasm.Web.Types
js_const_SVGLength_SVG_LENGTHTYPE_UNKNOWN :: Word16
js_const_SVGLength_SVG_LENGTHTYPE_UNKNOWN = 0
js_const_SVGLength_SVG_LENGTHTYPE_NUMBER :: Word16
js_const_SVGLength_SVG_LENGTHTYPE_NUMBER = 1
js_const_SVGLength_SVG_LENGTHTYPE_PERCENTAGE :: Word16
js_const_SVGLength_SVG_LENGTHTYPE_PERCENTAGE = 2
js_const_SVGLength_SVG_LENGTHTYPE_EMS :: Word16
js_const_SVGLength_SVG_LENGTHTYPE_EMS = 3
js_const_SVGLength_SVG_LENGTHTYPE_EXS :: Word16
js_const_SVGLength_SVG_LENGTHTYPE_EXS = 4
js_const_SVGLength_SVG_LENGTHTYPE_PX :: Word16
js_const_SVGLength_SVG_LENGTHTYPE_PX = 5
js_const_SVGLength_SVG_LENGTHTYPE_CM :: Word16
js_const_SVGLength_SVG_LENGTHTYPE_CM = 6
js_const_SVGLength_SVG_LENGTHTYPE_MM :: Word16
js_const_SVGLength_SVG_LENGTHTYPE_MM = 7
js_const_SVGLength_SVG_LENGTHTYPE_IN :: Word16
js_const_SVGLength_SVG_LENGTHTYPE_IN = 8
js_const_SVGLength_SVG_LENGTHTYPE_PT :: Word16
js_const_SVGLength_SVG_LENGTHTYPE_PT = 9
js_const_SVGLength_SVG_LENGTHTYPE_PC :: Word16
js_const_SVGLength_SVG_LENGTHTYPE_PC = 10
foreign import javascript unsafe "$1.newValueSpecifiedUnits($2,$3)" js_fun_newValueSpecifiedUnits_short_float_undefined
  :: SVGLength -> (Word16 -> (Float -> (IO ())))
foreign import javascript unsafe "$1.convertToSpecifiedUnits($2)" js_fun_convertToSpecifiedUnits_short_undefined
  :: SVGLength -> (Word16 -> (IO ()))
foreign import javascript unsafe "$1.unitType" js_get_unitType
  :: SVGLength -> (IO Word16)
foreign import javascript unsafe "$1.value" js_get_value
  :: SVGLength -> (IO Float)
foreign import javascript unsafe "$1.value = $2" js_set_value
  :: SVGLength -> (Float -> (IO ()))
foreign import javascript unsafe "$1.valueInSpecifiedUnits" js_get_valueInSpecifiedUnits
  :: SVGLength -> (IO Float)
foreign import javascript unsafe "$1.valueInSpecifiedUnits = $2" js_set_valueInSpecifiedUnits
  :: SVGLength -> (Float -> (IO ()))
foreign import javascript unsafe "$1.valueAsString" js_get_valueAsString
  :: SVGLength -> (IO DOMString)
foreign import javascript unsafe "$1.valueAsString = $2" js_set_valueAsString
  :: SVGLength -> (DOMString -> (IO ()))
