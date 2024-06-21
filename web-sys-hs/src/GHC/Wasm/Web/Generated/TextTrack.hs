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
module GHC.Wasm.Web.Generated.TextTrack (
        TextTrack, TextTrackClass, js_fun_addCue_VTTCue_undefined,
        js_fun_removeCue_VTTCue_undefined, js_get_kind, js_get_label,
        js_get_language, js_get_id, js_get_inBandMetadataTrackDispatchType,
        js_get_mode, js_set_mode, js_get_cues, js_get_activeCues,
        js_get_oncuechange, js_set_oncuechange, js_get_sourceBuffer
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.SourceBuffer.Core
import GHC.Wasm.Web.Generated.TextTrack.Core
import GHC.Wasm.Web.Generated.TextTrackCueList.Core
import GHC.Wasm.Web.Generated.TextTrackKind.Core
import GHC.Wasm.Web.Generated.TextTrackMode.Core
import GHC.Wasm.Web.Generated.VTTCue.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.addCue($2)" js_fun_addCue_VTTCue_undefined
  :: TextTrack -> (VTTCue -> (IO ()))
foreign import javascript unsafe "$1.removeCue($2)" js_fun_removeCue_VTTCue_undefined
  :: TextTrack -> (VTTCue -> (IO ()))
foreign import javascript unsafe "$1.kind" js_get_kind
  :: TextTrack -> (IO TextTrackKind)
foreign import javascript unsafe "$1.label" js_get_label
  :: TextTrack -> (IO DOMString)
foreign import javascript unsafe "$1.language" js_get_language
  :: TextTrack -> (IO DOMString)
foreign import javascript unsafe "$1.id" js_get_id
  :: TextTrack -> (IO DOMString)
foreign import javascript unsafe "$1.inBandMetadataTrackDispatchType" js_get_inBandMetadataTrackDispatchType
  :: TextTrack -> (IO DOMString)
foreign import javascript unsafe "$1.mode" js_get_mode
  :: TextTrack -> (IO TextTrackMode)
foreign import javascript unsafe "$1.mode = $2" js_set_mode
  :: TextTrack -> (TextTrackMode -> (IO ()))
foreign import javascript unsafe "$1.cues" js_get_cues
  :: TextTrack -> (IO (Nullable TextTrackCueListClass))
foreign import javascript unsafe "$1.activeCues" js_get_activeCues
  :: TextTrack -> (IO (Nullable TextTrackCueListClass))
foreign import javascript unsafe "$1.oncuechange" js_get_oncuechange
  :: TextTrack -> (IO EventHandler)
foreign import javascript unsafe "$1.oncuechange = $2" js_set_oncuechange
  :: TextTrack -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.sourceBuffer" js_get_sourceBuffer
  :: TextTrack -> (IO (Nullable SourceBufferClass))
