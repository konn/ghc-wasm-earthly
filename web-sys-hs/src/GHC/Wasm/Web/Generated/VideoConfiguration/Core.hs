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
module GHC.Wasm.Web.Generated.VideoConfiguration.Core (
        VideoConfigurationFields, VideoConfigurationClass,
        VideoConfiguration, ReifiedVideoConfiguration
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type VideoConfigurationFields =
    '[ '("bitrate", NullableClass (JSPrimClass Word64)),
       '("contentType", NullableClass DOMStringClass),
       '("framerate", NullableClass DOMStringClass),
       '("height", NullableClass (JSPrimClass Word32)),
       '("width", NullableClass (JSPrimClass Word32))]
type VideoConfigurationClass =
    JSDictionaryClass VideoConfigurationFields
type VideoConfiguration = JSObject VideoConfigurationClass
type ReifiedVideoConfiguration =
    ReifiedDictionary VideoConfigurationFields
