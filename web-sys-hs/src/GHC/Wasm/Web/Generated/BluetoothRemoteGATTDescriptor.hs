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
module GHC.Wasm.Web.Generated.BluetoothRemoteGATTDescriptor (
        BluetoothRemoteGATTDescriptor, BluetoothRemoteGATTDescriptorClass,
        js_fun_readValue__Promise_DataView,
        js_fun_writeValue_BufferSource_Promise_undefined,
        js_get_characteristic, js_get_uuid, js_get_value
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.BluetoothRemoteGATTCharacteristic.Core
import GHC.Wasm.Web.Generated.BluetoothRemoteGATTDescriptor.Core
import GHC.Wasm.Web.Generated.UUID.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.readValue()" js_fun_readValue__Promise_DataView
  :: BluetoothRemoteGATTDescriptor -> (IO (Promise DataViewClass))
foreign import javascript safe "$1.writeValue($2)" js_fun_writeValue_BufferSource_Promise_undefined
  :: BluetoothRemoteGATTDescriptor
     -> (BufferSource -> (IO (Promise UndefinedClass)))
foreign import javascript unsafe "$1.characteristic" js_get_characteristic
  :: BluetoothRemoteGATTDescriptor
     -> (IO BluetoothRemoteGATTCharacteristic)
foreign import javascript unsafe "$1.uuid" js_get_uuid
  :: BluetoothRemoteGATTDescriptor -> (IO UUID)
foreign import javascript unsafe "$1.value" js_get_value
  :: BluetoothRemoteGATTDescriptor -> (IO (Nullable DataViewClass))
