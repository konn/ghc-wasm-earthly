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
module GHC.Wasm.Web.Generated.VRLayer.Core (
        VRLayerFields, VRLayerClass, VRLayer, ReifiedVRLayer
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.HTMLCanvasElement.Core
import GHC.Wasm.Web.Types
type VRLayerFields =
    '[ '("leftBounds",
         NullableClass (SequenceClass (JSPrimClass Float))),
       '("rightBounds",
         NullableClass (SequenceClass (JSPrimClass Float))),
       '("source", NullableClass (NullableClass HTMLCanvasElementClass))]
type VRLayerClass = JSDictionaryClass VRLayerFields
type VRLayer = JSObject VRLayerClass
type ReifiedVRLayer = ReifiedDictionary VRLayerFields
