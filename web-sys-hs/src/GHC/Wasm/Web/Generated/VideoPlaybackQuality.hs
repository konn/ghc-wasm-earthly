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
module GHC.Wasm.Web.Generated.VideoPlaybackQuality (
        VideoPlaybackQuality, VideoPlaybackQualityClass,
        js_get_creationTime, js_get_totalVideoFrames,
        js_get_droppedVideoFrames, js_get_corruptedVideoFrames
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMHighResTimeStamp.Core
import GHC.Wasm.Web.Generated.VideoPlaybackQuality.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.creationTime" js_get_creationTime
  :: VideoPlaybackQuality -> (IO DOMHighResTimeStamp)
foreign import javascript unsafe "$1.totalVideoFrames" js_get_totalVideoFrames
  :: VideoPlaybackQuality -> (IO Word32)
foreign import javascript unsafe "$1.droppedVideoFrames" js_get_droppedVideoFrames
  :: VideoPlaybackQuality -> (IO Word32)
foreign import javascript unsafe "$1.corruptedVideoFrames" js_get_corruptedVideoFrames
  :: VideoPlaybackQuality -> (IO Word32)
