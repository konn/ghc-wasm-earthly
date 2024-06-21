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
module GHC.Wasm.Web.Generated.DisplayMediaStreamConstraints.Core (
        DisplayMediaStreamConstraintsFields,
        DisplayMediaStreamConstraintsClass, DisplayMediaStreamConstraints,
        ReifiedDisplayMediaStreamConstraints
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MediaTrackConstraints.Core
import GHC.Wasm.Web.Types
type DisplayMediaStreamConstraintsFields =
    '[ '("audio",
         NullableClass (UnionClass '[JSPrimClass Bool,
                                     MediaTrackConstraintsClass])),
       '("video",
         NullableClass (UnionClass '[JSPrimClass Bool,
                                     MediaTrackConstraintsClass]))]
type DisplayMediaStreamConstraintsClass =
    JSDictionaryClass DisplayMediaStreamConstraintsFields
type DisplayMediaStreamConstraints =
    JSObject DisplayMediaStreamConstraintsClass
type ReifiedDisplayMediaStreamConstraints =
    ReifiedDictionary DisplayMediaStreamConstraintsFields
