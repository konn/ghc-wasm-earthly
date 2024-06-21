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
module GHC.Wasm.Web.Generated.MediaDecodingConfiguration.Core (
        MediaDecodingConfigurationFields, MediaDecodingConfigurationClass,
        MediaDecodingConfiguration, ReifiedMediaDecodingConfiguration
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AudioConfiguration.Core
import GHC.Wasm.Web.Generated.MediaDecodingType.Core
import GHC.Wasm.Web.Generated.VideoConfiguration.Core
import GHC.Wasm.Web.Types
type MediaDecodingConfigurationFields =
    '[ '("audio", NullableClass AudioConfigurationClass),
       '("type", MediaDecodingTypeClass),
       '("video", NullableClass VideoConfigurationClass)]
type MediaDecodingConfigurationClass =
    JSDictionaryClass MediaDecodingConfigurationFields
type MediaDecodingConfiguration =
    JSObject MediaDecodingConfigurationClass
type ReifiedMediaDecodingConfiguration =
    ReifiedDictionary MediaDecodingConfigurationFields
