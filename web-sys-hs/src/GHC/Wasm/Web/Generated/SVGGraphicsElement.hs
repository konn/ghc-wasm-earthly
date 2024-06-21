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
module GHC.Wasm.Web.Generated.SVGGraphicsElement (
        SVGGraphicsElement, SVGGraphicsElementClass,
        js_fun_getBBox_nullable_SVGBoundingBoxOptions_SVGRect,
        js_fun_getCTM__nullable_SVGMatrix,
        js_fun_getScreenCTM__nullable_SVGMatrix,
        js_fun_getTransformToElement_SVGGraphicsElement_SVGMatrix,
        js_fun_hasExtension_DOMString_boolean, js_get_transform,
        js_get_nearestViewportElement, js_get_farthestViewportElement,
        js_get_requiredFeatures, js_get_requiredExtensions,
        js_get_systemLanguage
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.SVGAnimatedTransformList.Core
import GHC.Wasm.Web.Generated.SVGBoundingBoxOptions.Core
import GHC.Wasm.Web.Generated.SVGElement.Core
import GHC.Wasm.Web.Generated.SVGGraphicsElement.Core
import GHC.Wasm.Web.Generated.SVGMatrix.Core
import GHC.Wasm.Web.Generated.SVGRect.Core
import GHC.Wasm.Web.Generated.SVGStringList.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getBBox($2)" js_fun_getBBox_nullable_SVGBoundingBoxOptions_SVGRect
  :: SVGGraphicsElement
     -> (Nullable SVGBoundingBoxOptionsClass -> (IO SVGRect))
foreign import javascript unsafe "$1.getCTM()" js_fun_getCTM__nullable_SVGMatrix
  :: SVGGraphicsElement -> (IO (Nullable SVGMatrixClass))
foreign import javascript unsafe "$1.getScreenCTM()" js_fun_getScreenCTM__nullable_SVGMatrix
  :: SVGGraphicsElement -> (IO (Nullable SVGMatrixClass))
foreign import javascript unsafe "$1.getTransformToElement($2)" js_fun_getTransformToElement_SVGGraphicsElement_SVGMatrix
  :: SVGGraphicsElement -> (SVGGraphicsElement -> (IO SVGMatrix))
foreign import javascript unsafe "$1.hasExtension($2)" js_fun_hasExtension_DOMString_boolean
  :: SVGGraphicsElement -> (DOMString -> (IO Bool))
foreign import javascript unsafe "$1.transform" js_get_transform
  :: SVGGraphicsElement -> (IO SVGAnimatedTransformList)
foreign import javascript unsafe "$1.nearestViewportElement" js_get_nearestViewportElement
  :: SVGGraphicsElement -> (IO (Nullable SVGElementClass))
foreign import javascript unsafe "$1.farthestViewportElement" js_get_farthestViewportElement
  :: SVGGraphicsElement -> (IO (Nullable SVGElementClass))
foreign import javascript unsafe "$1.requiredFeatures" js_get_requiredFeatures
  :: SVGGraphicsElement -> (IO SVGStringList)
foreign import javascript unsafe "$1.requiredExtensions" js_get_requiredExtensions
  :: SVGGraphicsElement -> (IO SVGStringList)
foreign import javascript unsafe "$1.systemLanguage" js_get_systemLanguage
  :: SVGGraphicsElement -> (IO SVGStringList)
