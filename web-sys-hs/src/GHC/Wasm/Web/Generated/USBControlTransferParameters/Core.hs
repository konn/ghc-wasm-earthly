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
module GHC.Wasm.Web.Generated.USBControlTransferParameters.Core (
        USBControlTransferParametersFields,
        USBControlTransferParametersClass, USBControlTransferParameters,
        ReifiedUSBControlTransferParameters
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.USBRecipient.Core
import GHC.Wasm.Web.Generated.USBRequestType.Core
import GHC.Wasm.Web.Types
type USBControlTransferParametersFields =
    '[ '("index", JSPrimClass Word16),
       '("recipient", USBRecipientClass),
       '("request", JSPrimClass Word8),
       '("requestType", USBRequestTypeClass),
       '("value", JSPrimClass Word16)]
type USBControlTransferParametersClass =
    JSDictionaryClass USBControlTransferParametersFields
type USBControlTransferParameters =
    JSObject USBControlTransferParametersClass
type ReifiedUSBControlTransferParameters =
    ReifiedDictionary USBControlTransferParametersFields
