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
module GHC.Wasm.Web.Generated.VRDisplay (
        VRDisplay, VRDisplayClass,
        js_fun_getEyeParameters_VREye_VREyeParameters,
        js_fun_getFrameData_VRFrameData_boolean, js_fun_getPose__VRPose,
        js_fun_getSubmitFrameResult_VRSubmitFrameResult_boolean,
        js_fun_resetPose__undefined,
        js_fun_requestAnimationFrame_FrameRequestCallback_long,
        js_fun_cancelAnimationFrame_long_undefined,
        js_fun_requestPresent_sequence_VRLayer_Promise_undefined,
        js_fun_exitPresent__Promise_undefined,
        js_fun_getLayers__sequence_VRLayer, js_fun_submitFrame__undefined,
        js_get_presentingGroups, js_get_groupMask, js_set_groupMask,
        js_get_isConnected, js_get_isPresenting, js_get_capabilities,
        js_get_stageParameters, js_get_displayId, js_get_displayName,
        js_get_depthNear, js_set_depthNear, js_get_depthFar,
        js_set_depthFar
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.FrameRequestCallback.Core
import GHC.Wasm.Web.Generated.VRDisplay.Core
import GHC.Wasm.Web.Generated.VRDisplayCapabilities.Core
import GHC.Wasm.Web.Generated.VREye.Core
import GHC.Wasm.Web.Generated.VREyeParameters.Core
import GHC.Wasm.Web.Generated.VRFrameData.Core
import GHC.Wasm.Web.Generated.VRLayer.Core
import GHC.Wasm.Web.Generated.VRPose.Core
import GHC.Wasm.Web.Generated.VRStageParameters.Core
import GHC.Wasm.Web.Generated.VRSubmitFrameResult.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getEyeParameters($2)" js_fun_getEyeParameters_VREye_VREyeParameters
  :: VRDisplay -> (VREye -> (IO VREyeParameters))
foreign import javascript unsafe "$1.getFrameData($2)" js_fun_getFrameData_VRFrameData_boolean
  :: VRDisplay -> (VRFrameData -> (IO Bool))
foreign import javascript unsafe "$1.getPose()" js_fun_getPose__VRPose
  :: VRDisplay -> (IO VRPose)
foreign import javascript unsafe "$1.getSubmitFrameResult($2)" js_fun_getSubmitFrameResult_VRSubmitFrameResult_boolean
  :: VRDisplay -> (VRSubmitFrameResult -> (IO Bool))
foreign import javascript unsafe "$1.resetPose()" js_fun_resetPose__undefined
  :: VRDisplay -> (IO ())
foreign import javascript unsafe "$1.requestAnimationFrame($2)" js_fun_requestAnimationFrame_FrameRequestCallback_long
  :: VRDisplay -> (FrameRequestCallback -> (IO Int32))
foreign import javascript unsafe "$1.cancelAnimationFrame($2)" js_fun_cancelAnimationFrame_long_undefined
  :: VRDisplay -> (Int32 -> (IO ()))
foreign import javascript safe "$1.requestPresent($2)" js_fun_requestPresent_sequence_VRLayer_Promise_undefined
  :: VRDisplay
     -> (Sequence VRLayerClass -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.exitPresent()" js_fun_exitPresent__Promise_undefined
  :: VRDisplay -> (IO (Promise UndefinedClass))
foreign import javascript unsafe "$1.getLayers()" js_fun_getLayers__sequence_VRLayer
  :: VRDisplay -> (IO (Sequence VRLayerClass))
foreign import javascript unsafe "$1.submitFrame()" js_fun_submitFrame__undefined
  :: VRDisplay -> (IO ())
foreign import javascript unsafe "$1.presentingGroups" js_get_presentingGroups
  :: VRDisplay -> (IO Word32)
foreign import javascript unsafe "$1.groupMask" js_get_groupMask
  :: VRDisplay -> (IO Word32)
foreign import javascript unsafe "$1.groupMask = $2" js_set_groupMask
  :: VRDisplay -> (Word32 -> (IO ()))
foreign import javascript unsafe "$1.isConnected" js_get_isConnected
  :: VRDisplay -> (IO Bool)
foreign import javascript unsafe "$1.isPresenting" js_get_isPresenting
  :: VRDisplay -> (IO Bool)
foreign import javascript unsafe "$1.capabilities" js_get_capabilities
  :: VRDisplay -> (IO VRDisplayCapabilities)
foreign import javascript unsafe "$1.stageParameters" js_get_stageParameters
  :: VRDisplay -> (IO (Nullable VRStageParametersClass))
foreign import javascript unsafe "$1.displayId" js_get_displayId
  :: VRDisplay -> (IO Word32)
foreign import javascript unsafe "$1.displayName" js_get_displayName
  :: VRDisplay -> (IO DOMString)
foreign import javascript unsafe "$1.depthNear" js_get_depthNear
  :: VRDisplay -> (IO Double)
foreign import javascript unsafe "$1.depthNear = $2" js_set_depthNear
  :: VRDisplay -> (Double -> (IO ()))
foreign import javascript unsafe "$1.depthFar" js_get_depthFar
  :: VRDisplay -> (IO Double)
foreign import javascript unsafe "$1.depthFar = $2" js_set_depthFar
  :: VRDisplay -> (Double -> (IO ()))
