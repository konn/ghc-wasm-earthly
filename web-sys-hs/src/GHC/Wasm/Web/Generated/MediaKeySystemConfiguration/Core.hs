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
module GHC.Wasm.Web.Generated.MediaKeySystemConfiguration.Core (
        MediaKeySystemConfigurationFields,
        MediaKeySystemConfigurationClass, MediaKeySystemConfiguration,
        ReifiedMediaKeySystemConfiguration
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MediaKeySystemMediaCapability.Core
import GHC.Wasm.Web.Generated.MediaKeysRequirement.Core
import GHC.Wasm.Web.Types
type MediaKeySystemConfigurationFields =
    '[ '("audioCapabilities",
         NullableClass (SequenceClass MediaKeySystemMediaCapabilityClass)),
       '("distinctiveIdentifier",
         NullableClass MediaKeysRequirementClass),
       '("initDataTypes", NullableClass (SequenceClass DOMStringClass)),
       '("label", NullableClass DOMStringClass),
       '("persistentState", NullableClass MediaKeysRequirementClass),
       '("sessionTypes", NullableClass (SequenceClass DOMStringClass)),
       '("videoCapabilities",
         NullableClass (SequenceClass MediaKeySystemMediaCapabilityClass))]
type MediaKeySystemConfigurationClass =
    JSDictionaryClass MediaKeySystemConfigurationFields
type MediaKeySystemConfiguration =
    JSObject MediaKeySystemConfigurationClass
type ReifiedMediaKeySystemConfiguration =
    ReifiedDictionary MediaKeySystemConfigurationFields
