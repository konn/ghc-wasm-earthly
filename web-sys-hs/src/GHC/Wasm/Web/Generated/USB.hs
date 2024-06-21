{-# OPTIONS_GHC -Wno-unused-imports #-}
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
module GHC.Wasm.Web.Generated.USB (
        USB, USBClass, js_fun_getDevices__Promise_sequence_USBDevice,
        js_fun_requestDevice_USBDeviceRequestOptions_Promise_USBDevice,
        js_get_onconnect, js_set_onconnect, js_get_ondisconnect,
        js_set_ondisconnect
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.USB.Core
import GHC.Wasm.Web.Generated.USBDevice.Core
import GHC.Wasm.Web.Generated.USBDeviceRequestOptions.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.getDevices()" js_fun_getDevices__Promise_sequence_USBDevice
  :: USB -> (IO (Promise (SequenceClass USBDeviceClass)))
foreign import javascript safe "$1.requestDevice($2)" js_fun_requestDevice_USBDeviceRequestOptions_Promise_USBDevice
  :: USB
     -> (USBDeviceRequestOptions -> (IO (Promise USBDeviceClass)))
foreign import javascript unsafe "$1.onconnect" js_get_onconnect
  :: USB -> (IO EventHandler)
foreign import javascript unsafe "$1.onconnect = $2" js_set_onconnect
  :: USB -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondisconnect" js_get_ondisconnect
  :: USB -> (IO EventHandler)
foreign import javascript unsafe "$1.ondisconnect = $2" js_set_ondisconnect
  :: USB -> (EventHandler -> (IO ()))
