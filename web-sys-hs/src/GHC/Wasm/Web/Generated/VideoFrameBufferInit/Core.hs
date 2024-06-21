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
module GHC.Wasm.Web.Generated.VideoFrameBufferInit.Core (
        VideoFrameBufferInitFields, VideoFrameBufferInitClass,
        VideoFrameBufferInit, ReifiedVideoFrameBufferInit
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMRectInit.Core
import GHC.Wasm.Web.Generated.PlaneLayout.Core
import GHC.Wasm.Web.Generated.VideoColorSpaceInit.Core
import GHC.Wasm.Web.Generated.VideoPixelFormat.Core
import GHC.Wasm.Web.Types
type VideoFrameBufferInitFields =
    '[ '("codedHeight", JSPrimClass Word32),
       '("codedWidth", JSPrimClass Word32),
       '("colorSpace", NullableClass VideoColorSpaceInitClass),
       '("displayHeight", NullableClass (JSPrimClass Word32)),
       '("displayWidth", NullableClass (JSPrimClass Word32)),
       '("duration", NullableClass (JSPrimClass Word64)),
       '("format", VideoPixelFormatClass),
       '("layout", NullableClass (SequenceClass PlaneLayoutClass)),
       '("timestamp", JSPrimClass Int64),
       '("visibleRect", NullableClass DOMRectInitClass)]
type VideoFrameBufferInitClass =
    JSDictionaryClass VideoFrameBufferInitFields
type VideoFrameBufferInit = JSObject VideoFrameBufferInitClass
type ReifiedVideoFrameBufferInit =
    ReifiedDictionary VideoFrameBufferInitFields
