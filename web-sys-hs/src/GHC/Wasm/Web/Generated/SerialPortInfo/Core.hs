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
module GHC.Wasm.Web.Generated.SerialPortInfo.Core (
        SerialPortInfoFields, SerialPortInfoClass, SerialPortInfo,
        ReifiedSerialPortInfo
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type SerialPortInfoFields =
    '[ '("usbProductId", NullableClass (JSPrimClass Word16)),
       '("usbVendorId", NullableClass (JSPrimClass Word16))]
type SerialPortInfoClass = JSDictionaryClass SerialPortInfoFields
type SerialPortInfo = JSObject SerialPortInfoClass
type ReifiedSerialPortInfo = ReifiedDictionary SerialPortInfoFields
