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
module GHC.Wasm.Web.Generated.BluetoothRemoteGATTServer (
        BluetoothRemoteGATTServer, BluetoothRemoteGATTServerClass,
        js_fun_connect__Promise_BluetoothRemoteGATTServer,
        js_fun_disconnect__undefined,
        js_fun_getPrimaryService_BluetoothServiceUUID_Promise_BluetoothRemoteGATTService,
        js_fun_getPrimaryServices_nullable_BluetoothServiceUUID_Promise_sequence_BluetoothRemoteGATTService,
        js_get_device, js_get_connected
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.BluetoothDevice.Core
import GHC.Wasm.Web.Generated.BluetoothRemoteGATTServer.Core
import GHC.Wasm.Web.Generated.BluetoothRemoteGATTService.Core
import GHC.Wasm.Web.Generated.BluetoothServiceUUID.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.connect()" js_fun_connect__Promise_BluetoothRemoteGATTServer
  :: BluetoothRemoteGATTServer
     -> (IO (Promise BluetoothRemoteGATTServerClass))
foreign import javascript unsafe "$1.disconnect()" js_fun_disconnect__undefined
  :: BluetoothRemoteGATTServer -> (IO ())
foreign import javascript safe "$1.getPrimaryService($2)" js_fun_getPrimaryService_BluetoothServiceUUID_Promise_BluetoothRemoteGATTService
  :: BluetoothRemoteGATTServer
     -> (BluetoothServiceUUID
         -> (IO (Promise BluetoothRemoteGATTServiceClass)))
foreign import javascript safe "$1.getPrimaryServices($2)" js_fun_getPrimaryServices_nullable_BluetoothServiceUUID_Promise_sequence_BluetoothRemoteGATTService
  :: BluetoothRemoteGATTServer
     -> (Nullable BluetoothServiceUUIDClass
         -> (IO (Promise (SequenceClass BluetoothRemoteGATTServiceClass))))
foreign import javascript unsafe "$1.device" js_get_device
  :: BluetoothRemoteGATTServer -> (IO BluetoothDevice)
foreign import javascript unsafe "$1.connected" js_get_connected
  :: BluetoothRemoteGATTServer -> (IO Bool)
