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
module GHC.Wasm.Web.Generated.AudioConfiguration.Core (
        AudioConfigurationFields, AudioConfigurationClass,
        AudioConfiguration, ReifiedAudioConfiguration
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type AudioConfigurationFields =
    '[ '("bitrate", NullableClass (JSPrimClass Word64)),
       '("channels", NullableClass DOMStringClass),
       '("contentType", NullableClass DOMStringClass),
       '("samplerate", NullableClass (JSPrimClass Word32))]
type AudioConfigurationClass =
    JSDictionaryClass AudioConfigurationFields
type AudioConfiguration = JSObject AudioConfigurationClass
type ReifiedAudioConfiguration =
    ReifiedDictionary AudioConfigurationFields
