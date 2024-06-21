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
module GHC.Wasm.Web.Generated.TextTrackCue (
        TextTrackCue, TextTrackCueClass, js_get_track, js_get_id,
        js_set_id, js_get_startTime, js_set_startTime, js_get_endTime,
        js_set_endTime, js_get_pauseOnExit, js_set_pauseOnExit,
        js_get_onenter, js_set_onenter, js_get_onexit, js_set_onexit
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.TextTrack.Core
import GHC.Wasm.Web.Generated.TextTrackCue.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.track" js_get_track
  :: TextTrackCue -> (IO (Nullable TextTrackClass))
foreign import javascript unsafe "$1.id" js_get_id
  :: TextTrackCue -> (IO DOMString)
foreign import javascript unsafe "$1.id = $2" js_set_id
  :: TextTrackCue -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.startTime" js_get_startTime
  :: TextTrackCue -> (IO Double)
foreign import javascript unsafe "$1.startTime = $2" js_set_startTime
  :: TextTrackCue -> (Double -> (IO ()))
foreign import javascript unsafe "$1.endTime" js_get_endTime
  :: TextTrackCue -> (IO Double)
foreign import javascript unsafe "$1.endTime = $2" js_set_endTime
  :: TextTrackCue -> (Double -> (IO ()))
foreign import javascript unsafe "$1.pauseOnExit" js_get_pauseOnExit
  :: TextTrackCue -> (IO Bool)
foreign import javascript unsafe "$1.pauseOnExit = $2" js_set_pauseOnExit
  :: TextTrackCue -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.onenter" js_get_onenter
  :: TextTrackCue -> (IO EventHandler)
foreign import javascript unsafe "$1.onenter = $2" js_set_onenter
  :: TextTrackCue -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onexit" js_get_onexit
  :: TextTrackCue -> (IO EventHandler)
foreign import javascript unsafe "$1.onexit = $2" js_set_onexit
  :: TextTrackCue -> (EventHandler -> (IO ()))
