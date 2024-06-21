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
module GHC.Wasm.Web.Generated.VideoFrame (
        VideoFrame, VideoFrameClass,
        js_cons_VideoFrame_CanvasImageSource_nullable_VideoFrameInit,
        js_cons_VideoFrame_BufferSource_VideoFrameBufferInit,
        js_fun_allocationSize_nullable_VideoFrameCopyToOptions_long,
        js_fun_copyTo_BufferSource_nullable_VideoFrameCopyToOptions_Promise_sequence_PlaneLayout,
        js_fun_clone__VideoFrame, js_fun_close__undefined, js_get_format,
        js_get_codedWidth, js_get_codedHeight, js_get_codedRect,
        js_get_visibleRect, js_get_displayWidth, js_get_displayHeight,
        js_get_duration, js_get_timestamp, js_get_colorSpace
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.CanvasImageSource.Core
import GHC.Wasm.Web.Generated.DOMRectReadOnly.Core
import GHC.Wasm.Web.Generated.PlaneLayout.Core
import GHC.Wasm.Web.Generated.VideoColorSpace.Core
import GHC.Wasm.Web.Generated.VideoFrame.Core
import GHC.Wasm.Web.Generated.VideoFrameBufferInit.Core
import GHC.Wasm.Web.Generated.VideoFrameCopyToOptions.Core
import GHC.Wasm.Web.Generated.VideoFrameInit.Core
import GHC.Wasm.Web.Generated.VideoPixelFormat.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new VideoFrame($1,$2)" js_cons_VideoFrame_CanvasImageSource_nullable_VideoFrameInit
  :: CanvasImageSource
     -> (Nullable VideoFrameInitClass -> (IO VideoFrame))
foreign import javascript unsafe "new VideoFrame($1,$2)" js_cons_VideoFrame_BufferSource_VideoFrameBufferInit
  :: BufferSource -> (VideoFrameBufferInit -> (IO VideoFrame))
foreign import javascript unsafe "$1.allocationSize($2)" js_fun_allocationSize_nullable_VideoFrameCopyToOptions_long
  :: VideoFrame
     -> (Nullable VideoFrameCopyToOptionsClass -> (IO Word32))
foreign import javascript safe "$1.copyTo($2,$3)" js_fun_copyTo_BufferSource_nullable_VideoFrameCopyToOptions_Promise_sequence_PlaneLayout
  :: VideoFrame
     -> (BufferSource
         -> (Nullable VideoFrameCopyToOptionsClass
             -> (IO (Promise (SequenceClass PlaneLayoutClass)))))
foreign import javascript unsafe "$1.clone()" js_fun_clone__VideoFrame
  :: VideoFrame -> (IO VideoFrame)
foreign import javascript unsafe "$1.close()" js_fun_close__undefined
  :: VideoFrame -> (IO ())
foreign import javascript unsafe "$1.format" js_get_format
  :: VideoFrame -> (IO (Nullable VideoPixelFormatClass))
foreign import javascript unsafe "$1.codedWidth" js_get_codedWidth
  :: VideoFrame -> (IO Word32)
foreign import javascript unsafe "$1.codedHeight" js_get_codedHeight
  :: VideoFrame -> (IO Word32)
foreign import javascript unsafe "$1.codedRect" js_get_codedRect
  :: VideoFrame -> (IO (Nullable DOMRectReadOnlyClass))
foreign import javascript unsafe "$1.visibleRect" js_get_visibleRect
  :: VideoFrame -> (IO (Nullable DOMRectReadOnlyClass))
foreign import javascript unsafe "$1.displayWidth" js_get_displayWidth
  :: VideoFrame -> (IO Word32)
foreign import javascript unsafe "$1.displayHeight" js_get_displayHeight
  :: VideoFrame -> (IO Word32)
foreign import javascript unsafe "$1.duration" js_get_duration
  :: VideoFrame -> (IO (Nullable (JSPrimClass Word64)))
foreign import javascript unsafe "$1.timestamp" js_get_timestamp
  :: VideoFrame -> (IO (Nullable (JSPrimClass Int64)))
foreign import javascript unsafe "$1.colorSpace" js_get_colorSpace
  :: VideoFrame -> (IO VideoColorSpace)
