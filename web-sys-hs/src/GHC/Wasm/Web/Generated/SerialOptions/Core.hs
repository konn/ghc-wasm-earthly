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
module GHC.Wasm.Web.Generated.SerialOptions.Core (
        SerialOptionsFields, SerialOptionsClass, SerialOptions,
        ReifiedSerialOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.FlowControlType.Core
import GHC.Wasm.Web.Generated.ParityType.Core
import GHC.Wasm.Web.Types
type SerialOptionsFields =
    '[ '("baudRate", JSPrimClass Word32),
       '("bufferSize", NullableClass (JSPrimClass Word32)),
       '("dataBits", NullableClass (JSPrimClass Word8)),
       '("flowControl", NullableClass FlowControlTypeClass),
       '("parity", NullableClass ParityTypeClass),
       '("stopBits", NullableClass (JSPrimClass Word8))]
type SerialOptionsClass = JSDictionaryClass SerialOptionsFields
type SerialOptions = JSObject SerialOptionsClass
type ReifiedSerialOptions = ReifiedDictionary SerialOptionsFields
