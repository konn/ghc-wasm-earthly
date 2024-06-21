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
module GHC.Wasm.Web.Generated.MediaStreamConstraints.Core (
        MediaStreamConstraintsFields, MediaStreamConstraintsClass,
        MediaStreamConstraints, ReifiedMediaStreamConstraints
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MediaTrackConstraints.Core
import GHC.Wasm.Web.Types
type MediaStreamConstraintsFields =
    '[ '("audio",
         NullableClass (UnionClass '[JSPrimClass Bool,
                                     MediaTrackConstraintsClass])),
       '("fake", NullableClass (JSPrimClass Bool)),
       '("peerIdentity", NullableClass (NullableClass DOMStringClass)),
       '("picture", NullableClass (JSPrimClass Bool)),
       '("video",
         NullableClass (UnionClass '[JSPrimClass Bool,
                                     MediaTrackConstraintsClass]))]
type MediaStreamConstraintsClass =
    JSDictionaryClass MediaStreamConstraintsFields
type MediaStreamConstraints = JSObject MediaStreamConstraintsClass
type ReifiedMediaStreamConstraints =
    ReifiedDictionary MediaStreamConstraintsFields
