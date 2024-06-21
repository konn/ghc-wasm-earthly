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
module GHC.Wasm.Web.Generated.MediaTrackSettings.Core (
        MediaTrackSettingsFields, MediaTrackSettingsClass,
        MediaTrackSettings, ReifiedMediaTrackSettings
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type MediaTrackSettingsFields =
    '[ '("autoGainControl", NullableClass (JSPrimClass Bool)),
       '("channelCount", NullableClass (JSPrimClass Int32)),
       '("deviceId", NullableClass DOMStringClass),
       '("echoCancellation", NullableClass (JSPrimClass Bool)),
       '("facingMode", NullableClass DOMStringClass),
       '("frameRate", NullableClass (JSPrimClass Double)),
       '("height", NullableClass (JSPrimClass Int32)),
       '("noiseSuppression", NullableClass (JSPrimClass Bool)),
       '("width", NullableClass (JSPrimClass Int32))]
type MediaTrackSettingsClass =
    JSDictionaryClass MediaTrackSettingsFields
type MediaTrackSettings = JSObject MediaTrackSettingsClass
type ReifiedMediaTrackSettings =
    ReifiedDictionary MediaTrackSettingsFields
