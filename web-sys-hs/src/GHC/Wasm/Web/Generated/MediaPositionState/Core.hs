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
module GHC.Wasm.Web.Generated.MediaPositionState.Core (
        MediaPositionStateFields, MediaPositionStateClass,
        MediaPositionState, ReifiedMediaPositionState
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type MediaPositionStateFields =
    '[ '("duration", NullableClass (JSPrimClass Double)),
       '("playbackRate", NullableClass (JSPrimClass Double)),
       '("position", NullableClass (JSPrimClass Double))]
type MediaPositionStateClass =
    JSDictionaryClass MediaPositionStateFields
type MediaPositionState = JSObject MediaPositionStateClass
type ReifiedMediaPositionState =
    ReifiedDictionary MediaPositionStateFields
