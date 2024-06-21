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
module GHC.Wasm.Web.Generated.USBDeviceRequestOptions.Core (
        USBDeviceRequestOptionsFields, USBDeviceRequestOptionsClass,
        USBDeviceRequestOptions, ReifiedUSBDeviceRequestOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.USBDeviceFilter.Core
import GHC.Wasm.Web.Types
type USBDeviceRequestOptionsFields =
    '[ '("filters", SequenceClass USBDeviceFilterClass)]
type USBDeviceRequestOptionsClass =
    JSDictionaryClass USBDeviceRequestOptionsFields
type USBDeviceRequestOptions =
    JSObject USBDeviceRequestOptionsClass
type ReifiedUSBDeviceRequestOptions =
    ReifiedDictionary USBDeviceRequestOptionsFields
