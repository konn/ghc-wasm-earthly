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
module GHC.Wasm.Web.Generated.BluetoothRemoteGATTCharacteristic (
        BluetoothRemoteGATTCharacteristic,
        BluetoothRemoteGATTCharacteristicClass,
        js_fun_getDescriptor_BluetoothDescriptorUUID_Promise_BluetoothRemoteGATTDescriptor,
        js_fun_getDescriptors_nullable_BluetoothDescriptorUUID_Promise_sequence_BluetoothRemoteGATTDescriptor,
        js_fun_readValue__Promise_DataView,
        js_fun_writeValue_BufferSource_Promise_undefined,
        js_fun_writeValueWithResponse_BufferSource_Promise_undefined,
        js_fun_writeValueWithoutResponse_BufferSource_Promise_undefined,
        js_fun_startNotifications__Promise_BluetoothRemoteGATTCharacteristic,
        js_fun_stopNotifications__Promise_BluetoothRemoteGATTCharacteristic,
        js_get_service, js_get_uuid, js_get_properties, js_get_value,
        js_get_oncharacteristicvaluechanged,
        js_set_oncharacteristicvaluechanged
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.BluetoothCharacteristicProperties.Core
import GHC.Wasm.Web.Generated.BluetoothDescriptorUUID.Core
import GHC.Wasm.Web.Generated.BluetoothRemoteGATTCharacteristic.Core
import GHC.Wasm.Web.Generated.BluetoothRemoteGATTDescriptor.Core
import GHC.Wasm.Web.Generated.BluetoothRemoteGATTService.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.UUID.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.getDescriptor($2)" js_fun_getDescriptor_BluetoothDescriptorUUID_Promise_BluetoothRemoteGATTDescriptor
  :: BluetoothRemoteGATTCharacteristic
     -> (BluetoothDescriptorUUID
         -> (IO (Promise BluetoothRemoteGATTDescriptorClass)))
foreign import javascript safe "$1.getDescriptors($2)" js_fun_getDescriptors_nullable_BluetoothDescriptorUUID_Promise_sequence_BluetoothRemoteGATTDescriptor
  :: BluetoothRemoteGATTCharacteristic
     -> (Nullable BluetoothDescriptorUUIDClass
         -> (IO (Promise (SequenceClass BluetoothRemoteGATTDescriptorClass))))
foreign import javascript safe "$1.readValue()" js_fun_readValue__Promise_DataView
  :: BluetoothRemoteGATTCharacteristic
     -> (IO (Promise DataViewClass))
foreign import javascript safe "$1.writeValue($2)" js_fun_writeValue_BufferSource_Promise_undefined
  :: BluetoothRemoteGATTCharacteristic
     -> (BufferSource -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.writeValueWithResponse($2)" js_fun_writeValueWithResponse_BufferSource_Promise_undefined
  :: BluetoothRemoteGATTCharacteristic
     -> (BufferSource -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.writeValueWithoutResponse($2)" js_fun_writeValueWithoutResponse_BufferSource_Promise_undefined
  :: BluetoothRemoteGATTCharacteristic
     -> (BufferSource -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.startNotifications()" js_fun_startNotifications__Promise_BluetoothRemoteGATTCharacteristic
  :: BluetoothRemoteGATTCharacteristic
     -> (IO (Promise BluetoothRemoteGATTCharacteristicClass))
foreign import javascript safe "$1.stopNotifications()" js_fun_stopNotifications__Promise_BluetoothRemoteGATTCharacteristic
  :: BluetoothRemoteGATTCharacteristic
     -> (IO (Promise BluetoothRemoteGATTCharacteristicClass))
foreign import javascript unsafe "$1.service" js_get_service
  :: BluetoothRemoteGATTCharacteristic
     -> (IO BluetoothRemoteGATTService)
foreign import javascript unsafe "$1.uuid" js_get_uuid
  :: BluetoothRemoteGATTCharacteristic -> (IO UUID)
foreign import javascript unsafe "$1.properties" js_get_properties
  :: BluetoothRemoteGATTCharacteristic
     -> (IO BluetoothCharacteristicProperties)
foreign import javascript unsafe "$1.value" js_get_value
  :: BluetoothRemoteGATTCharacteristic
     -> (IO (Nullable DataViewClass))
foreign import javascript unsafe "$1.oncharacteristicvaluechanged" js_get_oncharacteristicvaluechanged
  :: BluetoothRemoteGATTCharacteristic -> (IO EventHandler)
foreign import javascript unsafe "$1.oncharacteristicvaluechanged = $2" js_set_oncharacteristicvaluechanged
  :: BluetoothRemoteGATTCharacteristic -> (EventHandler -> (IO ()))
