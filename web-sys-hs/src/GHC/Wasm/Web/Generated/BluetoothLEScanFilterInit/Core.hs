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
module GHC.Wasm.Web.Generated.BluetoothLEScanFilterInit.Core (
        BluetoothLEScanFilterInitFields, BluetoothLEScanFilterInitClass,
        BluetoothLEScanFilterInit, ReifiedBluetoothLEScanFilterInit
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.BluetoothServiceUUID.Core
import GHC.Wasm.Web.Types
type BluetoothLEScanFilterInitFields =
    '[ '("manufacturerData", NullableClass AnyClass),
       '("name", NullableClass DOMStringClass),
       '("namePrefix", NullableClass DOMStringClass),
       '("serviceData", NullableClass AnyClass),
       '("services",
         NullableClass (SequenceClass BluetoothServiceUUIDClass))]
type BluetoothLEScanFilterInitClass =
    JSDictionaryClass BluetoothLEScanFilterInitFields
type BluetoothLEScanFilterInit =
    JSObject BluetoothLEScanFilterInitClass
type ReifiedBluetoothLEScanFilterInit =
    ReifiedDictionary BluetoothLEScanFilterInitFields
