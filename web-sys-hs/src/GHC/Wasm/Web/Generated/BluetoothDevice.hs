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
module GHC.Wasm.Web.Generated.BluetoothDevice (
        BluetoothDevice, BluetoothDeviceClass,
        js_fun_watchAdvertisements_nullable_WatchAdvertisementsOptions_Promise_undefined,
        js_get_id, js_get_name, js_get_gatt, js_get_watchingAdvertisements,
        js_get_onadvertisementreceived, js_set_onadvertisementreceived,
        js_get_ongattserverdisconnected, js_set_ongattserverdisconnected,
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
import GHC.Wasm.Web.Generated.BluetoothDevice.Core
import GHC.Wasm.Web.Generated.BluetoothRemoteGATTServer.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.WatchAdvertisementsOptions.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.watchAdvertisements($2)" js_fun_watchAdvertisements_nullable_WatchAdvertisementsOptions_Promise_undefined
  :: BluetoothDevice
     -> (Nullable WatchAdvertisementsOptionsClass
         -> (IO (Promise UndefinedClass)))
foreign import javascript unsafe "$1.id" js_get_id
  :: BluetoothDevice -> (IO DOMString)
foreign import javascript unsafe "$1.name" js_get_name
  :: BluetoothDevice -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.gatt" js_get_gatt
  :: BluetoothDevice
     -> (IO (Nullable BluetoothRemoteGATTServerClass))
foreign import javascript unsafe "$1.watchingAdvertisements" js_get_watchingAdvertisements
  :: BluetoothDevice -> (IO Bool)
foreign import javascript unsafe "$1.onadvertisementreceived" js_get_onadvertisementreceived
  :: BluetoothDevice -> (IO EventHandler)
foreign import javascript unsafe "$1.onadvertisementreceived = $2" js_set_onadvertisementreceived
  :: BluetoothDevice -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ongattserverdisconnected" js_get_ongattserverdisconnected
  :: BluetoothDevice -> (IO EventHandler)
foreign import javascript unsafe "$1.ongattserverdisconnected = $2" js_set_ongattserverdisconnected
  :: BluetoothDevice -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncharacteristicvaluechanged" js_get_oncharacteristicvaluechanged
  :: BluetoothDevice -> (IO EventHandler)
foreign import javascript unsafe "$1.oncharacteristicvaluechanged = $2" js_set_oncharacteristicvaluechanged
  :: BluetoothDevice -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onserviceadded" js_get_onserviceadded
  :: BluetoothDevice -> (IO EventHandler)
foreign import javascript unsafe "$1.onserviceadded = $2" js_set_onserviceadded
  :: BluetoothDevice -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onservicechanged" js_get_onservicechanged
  :: BluetoothDevice -> (IO EventHandler)
foreign import javascript unsafe "$1.onservicechanged = $2" js_set_onservicechanged
  :: BluetoothDevice -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onserviceremoved" js_get_onserviceremoved
  :: BluetoothDevice -> (IO EventHandler)
foreign import javascript unsafe "$1.onserviceremoved = $2" js_set_onserviceremoved
  :: BluetoothDevice -> (EventHandler -> (IO ()))
