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
module GHC.Wasm.Web.Generated.HTMLVideoElement (
        HTMLVideoElement, HTMLVideoElementClass,
        js_fun_getVideoPlaybackQuality__VideoPlaybackQuality, js_get_width,
        js_set_width, js_get_height, js_set_height, js_get_videoWidth,
        js_get_videoHeight, js_get_poster, js_set_poster
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.HTMLMediaElement.Core
import GHC.Wasm.Web.Generated.HTMLVideoElement.Core
import GHC.Wasm.Web.Generated.VideoPlaybackQuality.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getVideoPlaybackQuality()" js_fun_getVideoPlaybackQuality__VideoPlaybackQuality
  :: HTMLVideoElement -> (IO VideoPlaybackQuality)
foreign import javascript unsafe "$1.width" js_get_width
  :: HTMLVideoElement -> (IO Word32)
foreign import javascript unsafe "$1.width = $2" js_set_width
  :: HTMLVideoElement -> (Word32 -> (IO ()))
foreign import javascript unsafe "$1.height" js_get_height
  :: HTMLVideoElement -> (IO Word32)
foreign import javascript unsafe "$1.height = $2" js_set_height
  :: HTMLVideoElement -> (Word32 -> (IO ()))
foreign import javascript unsafe "$1.videoWidth" js_get_videoWidth
  :: HTMLVideoElement -> (IO Word32)
foreign import javascript unsafe "$1.videoHeight" js_get_videoHeight
  :: HTMLVideoElement -> (IO Word32)
foreign import javascript unsafe "$1.poster" js_get_poster
  :: HTMLVideoElement -> (IO DOMString)
foreign import javascript unsafe "$1.poster = $2" js_set_poster
  :: HTMLVideoElement -> (DOMString -> (IO ()))
