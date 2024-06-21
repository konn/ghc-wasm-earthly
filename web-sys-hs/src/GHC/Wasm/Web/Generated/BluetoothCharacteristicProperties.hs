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
module GHC.Wasm.Web.Generated.BluetoothCharacteristicProperties (
        BluetoothCharacteristicProperties,
        BluetoothCharacteristicPropertiesClass, js_get_broadcast,
        js_get_read, js_get_writeWithoutResponse, js_get_write,
        js_get_notify, js_get_indicate, js_get_authenticatedSignedWrites,
        js_get_reliableWrite, js_get_writableAuxiliaries
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.BluetoothCharacteristicProperties.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.broadcast" js_get_broadcast
  :: BluetoothCharacteristicProperties -> (IO Bool)
foreign import javascript unsafe "$1.read" js_get_read
  :: BluetoothCharacteristicProperties -> (IO Bool)
foreign import javascript unsafe "$1.writeWithoutResponse" js_get_writeWithoutResponse
  :: BluetoothCharacteristicProperties -> (IO Bool)
foreign import javascript unsafe "$1.write" js_get_write
  :: BluetoothCharacteristicProperties -> (IO Bool)
foreign import javascript unsafe "$1.notify" js_get_notify
  :: BluetoothCharacteristicProperties -> (IO Bool)
foreign import javascript unsafe "$1.indicate" js_get_indicate
  :: BluetoothCharacteristicProperties -> (IO Bool)
foreign import javascript unsafe "$1.authenticatedSignedWrites" js_get_authenticatedSignedWrites
  :: BluetoothCharacteristicProperties -> (IO Bool)
foreign import javascript unsafe "$1.reliableWrite" js_get_reliableWrite
  :: BluetoothCharacteristicProperties -> (IO Bool)
foreign import javascript unsafe "$1.writableAuxiliaries" js_get_writableAuxiliaries
  :: BluetoothCharacteristicProperties -> (IO Bool)
