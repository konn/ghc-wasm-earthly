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
module GHC.Wasm.Web.Generated.USBDeviceFilter.Core (
        USBDeviceFilterFields, USBDeviceFilterClass, USBDeviceFilter,
        ReifiedUSBDeviceFilter
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type USBDeviceFilterFields =
    '[ '("classCode", NullableClass (JSPrimClass Word8)),
       '("productId", NullableClass (JSPrimClass Word16)),
       '("protocolCode", NullableClass (JSPrimClass Word8)),
       '("serialNumber", NullableClass DOMStringClass),
       '("subclassCode", NullableClass (JSPrimClass Word8)),
       '("vendorId", NullableClass (JSPrimClass Word16))]
type USBDeviceFilterClass = JSDictionaryClass USBDeviceFilterFields
type USBDeviceFilter = JSObject USBDeviceFilterClass
type ReifiedUSBDeviceFilter =
    ReifiedDictionary USBDeviceFilterFields
