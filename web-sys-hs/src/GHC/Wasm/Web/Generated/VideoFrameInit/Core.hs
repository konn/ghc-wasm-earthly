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
module GHC.Wasm.Web.Generated.VideoFrameInit.Core (
        VideoFrameInitFields, VideoFrameInitClass, VideoFrameInit,
        ReifiedVideoFrameInit
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AlphaOption.Core
import GHC.Wasm.Web.Generated.DOMRectInit.Core
import GHC.Wasm.Web.Types
type VideoFrameInitFields =
    '[ '("alpha", NullableClass AlphaOptionClass),
       '("displayHeight", NullableClass (JSPrimClass Word32)),
       '("displayWidth", NullableClass (JSPrimClass Word32)),
       '("duration", NullableClass (JSPrimClass Word64)),
       '("timestamp", NullableClass (JSPrimClass Int64)),
       '("visibleRect", NullableClass DOMRectInitClass)]
type VideoFrameInitClass = JSDictionaryClass VideoFrameInitFields
type VideoFrameInit = JSObject VideoFrameInitClass
type ReifiedVideoFrameInit = ReifiedDictionary VideoFrameInitFields
