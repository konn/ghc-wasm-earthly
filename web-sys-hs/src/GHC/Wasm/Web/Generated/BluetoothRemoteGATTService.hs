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
module GHC.Wasm.Web.Generated.BluetoothRemoteGATTService (
        BluetoothRemoteGATTService, BluetoothRemoteGATTServiceClass,
        js_fun_getCharacteristic_BluetoothCharacteristicUUID_Promise_BluetoothRemoteGATTCharacteristic,
        js_fun_getCharacteristics_nullable_BluetoothCharacteristicUUID_Promise_sequence_BluetoothRemoteGATTCharacteristic,
        js_fun_getIncludedService_BluetoothServiceUUID_Promise_BluetoothRemoteGATTService,
        js_fun_getIncludedServices_nullable_BluetoothServiceUUID_Promise_sequence_BluetoothRemoteGATTService,
        js_get_device, js_get_uuid, js_get_isPrimary,
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
import GHC.Wasm.Web.Generated.BluetoothCharacteristicUUID.Core
import GHC.Wasm.Web.Generated.BluetoothDevice.Core
import GHC.Wasm.Web.Generated.BluetoothRemoteGATTCharacteristic.Core
import GHC.Wasm.Web.Generated.BluetoothRemoteGATTService.Core
import GHC.Wasm.Web.Generated.BluetoothServiceUUID.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.UUID.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.getCharacteristic($2)" js_fun_getCharacteristic_BluetoothCharacteristicUUID_Promise_BluetoothRemoteGATTCharacteristic
  :: BluetoothRemoteGATTService
     -> (BluetoothCharacteristicUUID
         -> (IO (Promise BluetoothRemoteGATTCharacteristicClass)))
foreign import javascript safe "$1.getCharacteristics($2)" js_fun_getCharacteristics_nullable_BluetoothCharacteristicUUID_Promise_sequence_BluetoothRemoteGATTCharacteristic
  :: BluetoothRemoteGATTService
     -> (Nullable BluetoothCharacteristicUUIDClass
         -> (IO (Promise (SequenceClass BluetoothRemoteGATTCharacteristicClass))))
foreign import javascript safe "$1.getIncludedService($2)" js_fun_getIncludedService_BluetoothServiceUUID_Promise_BluetoothRemoteGATTService
  :: BluetoothRemoteGATTService
     -> (BluetoothServiceUUID
         -> (IO (Promise BluetoothRemoteGATTServiceClass)))
foreign import javascript safe "$1.getIncludedServices($2)" js_fun_getIncludedServices_nullable_BluetoothServiceUUID_Promise_sequence_BluetoothRemoteGATTService
  :: BluetoothRemoteGATTService
     -> (Nullable BluetoothServiceUUIDClass
         -> (IO (Promise (SequenceClass BluetoothRemoteGATTServiceClass))))
foreign import javascript unsafe "$1.device" js_get_device
  :: BluetoothRemoteGATTService -> (IO BluetoothDevice)
foreign import javascript unsafe "$1.uuid" js_get_uuid
  :: BluetoothRemoteGATTService -> (IO UUID)
foreign import javascript unsafe "$1.isPrimary" js_get_isPrimary
  :: BluetoothRemoteGATTService -> (IO Bool)
foreign import javascript unsafe "$1.oncharacteristicvaluechanged" js_get_oncharacteristicvaluechanged
  :: BluetoothRemoteGATTService -> (IO EventHandler)
foreign import javascript unsafe "$1.oncharacteristicvaluechanged = $2" js_set_oncharacteristicvaluechanged
  :: BluetoothRemoteGATTService -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onserviceadded" js_get_onserviceadded
  :: BluetoothRemoteGATTService -> (IO EventHandler)
foreign import javascript unsafe "$1.onserviceadded = $2" js_set_onserviceadded
  :: BluetoothRemoteGATTService -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onservicechanged" js_get_onservicechanged
  :: BluetoothRemoteGATTService -> (IO EventHandler)
foreign import javascript unsafe "$1.onservicechanged = $2" js_set_onservicechanged
  :: BluetoothRemoteGATTService -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onserviceremoved" js_get_onserviceremoved
  :: BluetoothRemoteGATTService -> (IO EventHandler)
foreign import javascript unsafe "$1.onserviceremoved = $2" js_set_onserviceremoved
  :: BluetoothRemoteGATTService -> (EventHandler -> (IO ()))
