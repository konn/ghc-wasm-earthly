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
module GHC.Wasm.Web.Generated.CanvasImageSource.Core (
        CanvasImageSourceClass, CanvasImageSource
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.HTMLCanvasElement.Core
import GHC.Wasm.Web.Generated.HTMLOrSVGImageElement.Core
import GHC.Wasm.Web.Generated.HTMLVideoElement.Core
import GHC.Wasm.Web.Generated.ImageBitmap.Core
import GHC.Wasm.Web.Generated.OffscreenCanvas.Core
import GHC.Wasm.Web.Generated.VideoFrame.Core
import GHC.Wasm.Web.Types
type CanvasImageSourceClass =
    UnionClass '[HTMLOrSVGImageElementClass,
                 HTMLCanvasElementClass,
                 HTMLVideoElementClass,
                 ImageBitmapClass,
                 OffscreenCanvasClass,
                 VideoFrameClass]
type CanvasImageSource =
    JSObject (UnionClass '[HTMLOrSVGImageElementClass,
                           HTMLCanvasElementClass,
                           HTMLVideoElementClass,
                           ImageBitmapClass,
                           OffscreenCanvasClass,
                           VideoFrameClass])
