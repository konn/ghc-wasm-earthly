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
module GHC.Wasm.Web.Generated.SVGSVGElement (
        SVGSVGElement, SVGSVGElementClass,
        js_const_SVGSVGElement_SVG_ZOOMANDPAN_UNKNOWN,
        js_const_SVGSVGElement_SVG_ZOOMANDPAN_DISABLE,
        js_const_SVGSVGElement_SVG_ZOOMANDPAN_MAGNIFY,
        js_fun_suspendRedraw_long_long,
        js_fun_unsuspendRedraw_long_undefined,
        js_fun_unsuspendRedrawAll__undefined,
        js_fun_forceRedraw__undefined, js_fun_pauseAnimations__undefined,
        js_fun_unpauseAnimations__undefined,
        js_fun_animationsPaused__boolean, js_fun_getCurrentTime__float,
        js_fun_setCurrentTime_float_undefined,
        js_fun_deselectAll__undefined, js_fun_createSVGNumber__SVGNumber,
        js_fun_createSVGLength__SVGLength, js_fun_createSVGAngle__SVGAngle,
        js_fun_createSVGPoint__SVGPoint, js_fun_createSVGMatrix__SVGMatrix,
        js_fun_createSVGRect__SVGRect,
        js_fun_createSVGTransform__SVGTransform,
        js_fun_createSVGTransformFromMatrix_SVGMatrix_SVGTransform,
        js_fun_getElementById_DOMString_nullable_Element, js_get_x,
        js_get_y, js_get_width, js_get_height, js_get_useCurrentView,
        js_get_currentScale, js_set_currentScale, js_get_currentTranslate,
        js_get_viewBox, js_get_preserveAspectRatio, js_get_zoomAndPan,
        js_set_zoomAndPan
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Element.Core
import GHC.Wasm.Web.Generated.SVGAngle.Core
import GHC.Wasm.Web.Generated.SVGAnimatedLength.Core
import GHC.Wasm.Web.Generated.SVGAnimatedPreserveAspectRatio.Core
import GHC.Wasm.Web.Generated.SVGAnimatedRect.Core
import GHC.Wasm.Web.Generated.SVGGraphicsElement.Core
import GHC.Wasm.Web.Generated.SVGLength.Core
import GHC.Wasm.Web.Generated.SVGMatrix.Core
import GHC.Wasm.Web.Generated.SVGNumber.Core
import GHC.Wasm.Web.Generated.SVGPoint.Core
import GHC.Wasm.Web.Generated.SVGRect.Core
import GHC.Wasm.Web.Generated.SVGSVGElement.Core
import GHC.Wasm.Web.Generated.SVGTransform.Core
import GHC.Wasm.Web.Types
js_const_SVGSVGElement_SVG_ZOOMANDPAN_UNKNOWN :: Word16
js_const_SVGSVGElement_SVG_ZOOMANDPAN_UNKNOWN = 0
js_const_SVGSVGElement_SVG_ZOOMANDPAN_DISABLE :: Word16
js_const_SVGSVGElement_SVG_ZOOMANDPAN_DISABLE = 1
js_const_SVGSVGElement_SVG_ZOOMANDPAN_MAGNIFY :: Word16
js_const_SVGSVGElement_SVG_ZOOMANDPAN_MAGNIFY = 2
foreign import javascript unsafe "$1.suspendRedraw($2)" js_fun_suspendRedraw_long_long
  :: SVGSVGElement -> (Word32 -> (IO Word32))
foreign import javascript unsafe "$1.unsuspendRedraw($2)" js_fun_unsuspendRedraw_long_undefined
  :: SVGSVGElement -> (Word32 -> (IO ()))
foreign import javascript unsafe "$1.unsuspendRedrawAll()" js_fun_unsuspendRedrawAll__undefined
  :: SVGSVGElement -> (IO ())
foreign import javascript unsafe "$1.forceRedraw()" js_fun_forceRedraw__undefined
  :: SVGSVGElement -> (IO ())
foreign import javascript unsafe "$1.pauseAnimations()" js_fun_pauseAnimations__undefined
  :: SVGSVGElement -> (IO ())
foreign import javascript unsafe "$1.unpauseAnimations()" js_fun_unpauseAnimations__undefined
  :: SVGSVGElement -> (IO ())
foreign import javascript unsafe "$1.animationsPaused()" js_fun_animationsPaused__boolean
  :: SVGSVGElement -> (IO Bool)
foreign import javascript unsafe "$1.getCurrentTime()" js_fun_getCurrentTime__float
  :: SVGSVGElement -> (IO Float)
foreign import javascript unsafe "$1.setCurrentTime($2)" js_fun_setCurrentTime_float_undefined
  :: SVGSVGElement -> (Float -> (IO ()))
foreign import javascript unsafe "$1.deselectAll()" js_fun_deselectAll__undefined
  :: SVGSVGElement -> (IO ())
foreign import javascript unsafe "$1.createSVGNumber()" js_fun_createSVGNumber__SVGNumber
  :: SVGSVGElement -> (IO SVGNumber)
foreign import javascript unsafe "$1.createSVGLength()" js_fun_createSVGLength__SVGLength
  :: SVGSVGElement -> (IO SVGLength)
foreign import javascript unsafe "$1.createSVGAngle()" js_fun_createSVGAngle__SVGAngle
  :: SVGSVGElement -> (IO SVGAngle)
foreign import javascript unsafe "$1.createSVGPoint()" js_fun_createSVGPoint__SVGPoint
  :: SVGSVGElement -> (IO SVGPoint)
foreign import javascript unsafe "$1.createSVGMatrix()" js_fun_createSVGMatrix__SVGMatrix
  :: SVGSVGElement -> (IO SVGMatrix)
foreign import javascript unsafe "$1.createSVGRect()" js_fun_createSVGRect__SVGRect
  :: SVGSVGElement -> (IO SVGRect)
foreign import javascript unsafe "$1.createSVGTransform()" js_fun_createSVGTransform__SVGTransform
  :: SVGSVGElement -> (IO SVGTransform)
foreign import javascript unsafe "$1.createSVGTransformFromMatrix($2)" js_fun_createSVGTransformFromMatrix_SVGMatrix_SVGTransform
  :: SVGSVGElement -> (SVGMatrix -> (IO SVGTransform))
foreign import javascript unsafe "$1.getElementById($2)" js_fun_getElementById_DOMString_nullable_Element
  :: SVGSVGElement -> (DOMString -> (IO (Nullable ElementClass)))
foreign import javascript unsafe "$1.x" js_get_x
  :: SVGSVGElement -> (IO SVGAnimatedLength)
foreign import javascript unsafe "$1.y" js_get_y
  :: SVGSVGElement -> (IO SVGAnimatedLength)
foreign import javascript unsafe "$1.width" js_get_width
  :: SVGSVGElement -> (IO SVGAnimatedLength)
foreign import javascript unsafe "$1.height" js_get_height
  :: SVGSVGElement -> (IO SVGAnimatedLength)
foreign import javascript unsafe "$1.useCurrentView" js_get_useCurrentView
  :: SVGSVGElement -> (IO Bool)
foreign import javascript unsafe "$1.currentScale" js_get_currentScale
  :: SVGSVGElement -> (IO Float)
foreign import javascript unsafe "$1.currentScale = $2" js_set_currentScale
  :: SVGSVGElement -> (Float -> (IO ()))
foreign import javascript unsafe "$1.currentTranslate" js_get_currentTranslate
  :: SVGSVGElement -> (IO SVGPoint)
foreign import javascript unsafe "$1.viewBox" js_get_viewBox
  :: SVGSVGElement -> (IO SVGAnimatedRect)
foreign import javascript unsafe "$1.preserveAspectRatio" js_get_preserveAspectRatio
  :: SVGSVGElement -> (IO SVGAnimatedPreserveAspectRatio)
foreign import javascript unsafe "$1.zoomAndPan" js_get_zoomAndPan
  :: SVGSVGElement -> (IO Word16)
foreign import javascript unsafe "$1.zoomAndPan = $2" js_set_zoomAndPan
  :: SVGSVGElement -> (Word16 -> (IO ()))
