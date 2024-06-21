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
module GHC.Wasm.Web.Generated.RequestDeviceOptions.Core (
        RequestDeviceOptionsFields, RequestDeviceOptionsClass,
        RequestDeviceOptions, ReifiedRequestDeviceOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.BluetoothLEScanFilterInit.Core
import GHC.Wasm.Web.Generated.BluetoothServiceUUID.Core
import GHC.Wasm.Web.Types
type RequestDeviceOptionsFields =
    '[ '("acceptAllDevices", NullableClass (JSPrimClass Bool)),
       '("filters",
         NullableClass (SequenceClass BluetoothLEScanFilterInitClass)),
       '("optionalServices",
         NullableClass (SequenceClass BluetoothServiceUUIDClass))]
type RequestDeviceOptionsClass =
    JSDictionaryClass RequestDeviceOptionsFields
type RequestDeviceOptions = JSObject RequestDeviceOptionsClass
type ReifiedRequestDeviceOptions =
    ReifiedDictionary RequestDeviceOptionsFields
