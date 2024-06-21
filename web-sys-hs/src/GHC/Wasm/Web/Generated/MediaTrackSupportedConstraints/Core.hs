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
module GHC.Wasm.Web.Generated.MediaTrackSupportedConstraints.Core (
        MediaTrackSupportedConstraintsFields,
        MediaTrackSupportedConstraintsClass,
        MediaTrackSupportedConstraints,
        ReifiedMediaTrackSupportedConstraints
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type MediaTrackSupportedConstraintsFields =
    '[ '("aspectRatio", NullableClass (JSPrimClass Bool)),
       '("autoGainControl", NullableClass (JSPrimClass Bool)),
       '("channelCount", NullableClass (JSPrimClass Bool)),
       '("deviceId", NullableClass (JSPrimClass Bool)),
       '("echoCancellation", NullableClass (JSPrimClass Bool)),
       '("facingMode", NullableClass (JSPrimClass Bool)),
       '("frameRate", NullableClass (JSPrimClass Bool)),
       '("groupId", NullableClass (JSPrimClass Bool)),
       '("height", NullableClass (JSPrimClass Bool)),
       '("latency", NullableClass (JSPrimClass Bool)),
       '("noiseSuppression", NullableClass (JSPrimClass Bool)),
       '("sampleRate", NullableClass (JSPrimClass Bool)),
       '("sampleSize", NullableClass (JSPrimClass Bool)),
       '("volume", NullableClass (JSPrimClass Bool)),
       '("width", NullableClass (JSPrimClass Bool))]
type MediaTrackSupportedConstraintsClass =
    JSDictionaryClass MediaTrackSupportedConstraintsFields
type MediaTrackSupportedConstraints =
    JSObject MediaTrackSupportedConstraintsClass
type ReifiedMediaTrackSupportedConstraints =
    ReifiedDictionary MediaTrackSupportedConstraintsFields
