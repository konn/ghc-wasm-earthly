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
module GHC.Wasm.Web.Generated.Bluetooth (
        Bluetooth, BluetoothClass, js_fun_getAvailability__Promise_boolean,
        js_fun_getDevices__Promise_sequence_BluetoothDevice,
        js_fun_requestDevice_RequestDeviceOptions_Promise_BluetoothDevice,
        js_get_onavailabilitychanged, js_set_onavailabilitychanged,
        js_get_referringDevice, js_get_onadvertisementreceived,
        js_set_onadvertisementreceived, js_get_ongattserverdisconnected,
        js_set_ongattserverdisconnected,
        js_get_oncharacteristicvaluechanged,
        js_set_oncharacteristicvaluechanged, js_get_onserviceadded,
        js_set_onserviceadded, js_get_onservicechanged,
        js_set_onservicechanged, js_get_onserviceremoved,
        js_set_onserviceremoved
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Bluetooth.Core
import GHC.Wasm.Web.Generated.BluetoothDevice.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.RequestDeviceOptions.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.getAvailability()" js_fun_getAvailability__Promise_boolean
  :: Bluetooth -> (IO (Promise (JSPrimClass Bool)))
foreign import javascript safe "$1.getDevices()" js_fun_getDevices__Promise_sequence_BluetoothDevice
  :: Bluetooth -> (IO (Promise (SequenceClass BluetoothDeviceClass)))
foreign import javascript safe "$1.requestDevice($2)" js_fun_requestDevice_RequestDeviceOptions_Promise_BluetoothDevice
  :: Bluetooth
     -> (RequestDeviceOptions -> (IO (Promise BluetoothDeviceClass)))
foreign import javascript unsafe "$1.onavailabilitychanged" js_get_onavailabilitychanged
  :: Bluetooth -> (IO EventHandler)
foreign import javascript unsafe "$1.onavailabilitychanged = $2" js_set_onavailabilitychanged
  :: Bluetooth -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.referringDevice" js_get_referringDevice
  :: Bluetooth -> (IO (Nullable BluetoothDeviceClass))
foreign import javascript unsafe "$1.onadvertisementreceived" js_get_onadvertisementreceived
  :: Bluetooth -> (IO EventHandler)
foreign import javascript unsafe "$1.onadvertisementreceived = $2" js_set_onadvertisementreceived
  :: Bluetooth -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ongattserverdisconnected" js_get_ongattserverdisconnected
  :: Bluetooth -> (IO EventHandler)
foreign import javascript unsafe "$1.ongattserverdisconnected = $2" js_set_ongattserverdisconnected
  :: Bluetooth -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncharacteristicvaluechanged" js_get_oncharacteristicvaluechanged
  :: Bluetooth -> (IO EventHandler)
foreign import javascript unsafe "$1.oncharacteristicvaluechanged = $2" js_set_oncharacteristicvaluechanged
  :: Bluetooth -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onserviceadded" js_get_onserviceadded
  :: Bluetooth -> (IO EventHandler)
foreign import javascript unsafe "$1.onserviceadded = $2" js_set_onserviceadded
  :: Bluetooth -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onservicechanged" js_get_onservicechanged
  :: Bluetooth -> (IO EventHandler)
foreign import javascript unsafe "$1.onservicechanged = $2" js_set_onservicechanged
  :: Bluetooth -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onserviceremoved" js_get_onserviceremoved
  :: Bluetooth -> (IO EventHandler)
foreign import javascript unsafe "$1.onserviceremoved = $2" js_set_onserviceremoved
  :: Bluetooth -> (EventHandler -> (IO ()))
