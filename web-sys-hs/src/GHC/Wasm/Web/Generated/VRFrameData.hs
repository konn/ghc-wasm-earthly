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
module GHC.Wasm.Web.Generated.VRFrameData (
        VRFrameData, VRFrameDataClass, js_get_timestamp,
        js_get_leftProjectionMatrix, js_get_leftViewMatrix,
        js_get_rightProjectionMatrix, js_get_rightViewMatrix, js_get_pose
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMHighResTimeStamp.Core
import GHC.Wasm.Web.Generated.VRFrameData.Core
import GHC.Wasm.Web.Generated.VRPose.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.timestamp" js_get_timestamp
  :: VRFrameData -> (IO DOMHighResTimeStamp)
foreign import javascript unsafe "$1.leftProjectionMatrix" js_get_leftProjectionMatrix
  :: VRFrameData -> (IO (JSByteArray Float))
foreign import javascript unsafe "$1.leftViewMatrix" js_get_leftViewMatrix
  :: VRFrameData -> (IO (JSByteArray Float))
foreign import javascript unsafe "$1.rightProjectionMatrix" js_get_rightProjectionMatrix
  :: VRFrameData -> (IO (JSByteArray Float))
foreign import javascript unsafe "$1.rightViewMatrix" js_get_rightViewMatrix
  :: VRFrameData -> (IO (JSByteArray Float))
foreign import javascript unsafe "$1.pose" js_get_pose
  :: VRFrameData -> (IO VRPose)
