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
module GHC.Wasm.Web.Generated.MediaDevices (
        MediaDevices, MediaDevicesClass,
        js_fun_getSupportedConstraints__MediaTrackSupportedConstraints,
        js_fun_enumerateDevices__Promise_sequence_MediaDeviceInfo,
        js_fun_getUserMedia_nullable_MediaStreamConstraints_Promise_MediaStream,
        js_fun_getDisplayMedia_nullable_DisplayMediaStreamConstraints_Promise_MediaStream,
        js_get_ondevicechange, js_set_ondevicechange
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DisplayMediaStreamConstraints.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.MediaDeviceInfo.Core
import GHC.Wasm.Web.Generated.MediaDevices.Core
import GHC.Wasm.Web.Generated.MediaStream.Core
import GHC.Wasm.Web.Generated.MediaStreamConstraints.Core
import GHC.Wasm.Web.Generated.MediaTrackSupportedConstraints.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getSupportedConstraints()" js_fun_getSupportedConstraints__MediaTrackSupportedConstraints
  :: MediaDevices -> (IO MediaTrackSupportedConstraints)
foreign import javascript safe "$1.enumerateDevices()" js_fun_enumerateDevices__Promise_sequence_MediaDeviceInfo
  :: MediaDevices
     -> (IO (Promise (SequenceClass MediaDeviceInfoClass)))
foreign import javascript safe "$1.getUserMedia($2)" js_fun_getUserMedia_nullable_MediaStreamConstraints_Promise_MediaStream
  :: MediaDevices
     -> (Nullable MediaStreamConstraintsClass
         -> (IO (Promise MediaStreamClass)))
foreign import javascript safe "$1.getDisplayMedia($2)" js_fun_getDisplayMedia_nullable_DisplayMediaStreamConstraints_Promise_MediaStream
  :: MediaDevices
     -> (Nullable DisplayMediaStreamConstraintsClass
         -> (IO (Promise MediaStreamClass)))
foreign import javascript unsafe "$1.ondevicechange" js_get_ondevicechange
  :: MediaDevices -> (IO EventHandler)
foreign import javascript unsafe "$1.ondevicechange = $2" js_set_ondevicechange
  :: MediaDevices -> (EventHandler -> (IO ()))
