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
module GHC.Wasm.Web.Generated.MediaStreamTrack (
        MediaStreamTrack, MediaStreamTrackClass,
        js_fun_clone__MediaStreamTrack, js_fun_stop__undefined,
        js_fun_getConstraints__MediaTrackConstraints,
        js_fun_getSettings__MediaTrackSettings,
        js_fun_applyConstraints_nullable_MediaTrackConstraints_Promise_undefined,
        js_fun_mutedChanged_boolean_undefined, js_get_kind, js_get_id,
        js_get_label, js_get_enabled, js_set_enabled, js_get_muted,
        js_get_onmute, js_set_onmute, js_get_onunmute, js_set_onunmute,
        js_get_readyState, js_get_onended, js_set_onended
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.MediaStreamTrack.Core
import GHC.Wasm.Web.Generated.MediaStreamTrackState.Core
import GHC.Wasm.Web.Generated.MediaTrackConstraints.Core
import GHC.Wasm.Web.Generated.MediaTrackSettings.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.clone()" js_fun_clone__MediaStreamTrack
  :: MediaStreamTrack -> (IO MediaStreamTrack)
foreign import javascript unsafe "$1.stop()" js_fun_stop__undefined
  :: MediaStreamTrack -> (IO ())
foreign import javascript unsafe "$1.getConstraints()" js_fun_getConstraints__MediaTrackConstraints
  :: MediaStreamTrack -> (IO MediaTrackConstraints)
foreign import javascript unsafe "$1.getSettings()" js_fun_getSettings__MediaTrackSettings
  :: MediaStreamTrack -> (IO MediaTrackSettings)
foreign import javascript safe "$1.applyConstraints($2)" js_fun_applyConstraints_nullable_MediaTrackConstraints_Promise_undefined
  :: MediaStreamTrack
     -> (Nullable MediaTrackConstraintsClass
         -> (IO (Promise UndefinedClass)))
foreign import javascript unsafe "$1.mutedChanged($2)" js_fun_mutedChanged_boolean_undefined
  :: MediaStreamTrack -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.kind" js_get_kind
  :: MediaStreamTrack -> (IO DOMString)
foreign import javascript unsafe "$1.id" js_get_id
  :: MediaStreamTrack -> (IO DOMString)
foreign import javascript unsafe "$1.label" js_get_label
  :: MediaStreamTrack -> (IO DOMString)
foreign import javascript unsafe "$1.enabled" js_get_enabled
  :: MediaStreamTrack -> (IO Bool)
foreign import javascript unsafe "$1.enabled = $2" js_set_enabled
  :: MediaStreamTrack -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.muted" js_get_muted
  :: MediaStreamTrack -> (IO Bool)
foreign import javascript unsafe "$1.onmute" js_get_onmute
  :: MediaStreamTrack -> (IO EventHandler)
foreign import javascript unsafe "$1.onmute = $2" js_set_onmute
  :: MediaStreamTrack -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onunmute" js_get_onunmute
  :: MediaStreamTrack -> (IO EventHandler)
foreign import javascript unsafe "$1.onunmute = $2" js_set_onunmute
  :: MediaStreamTrack -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.readyState" js_get_readyState
  :: MediaStreamTrack -> (IO MediaStreamTrackState)
foreign import javascript unsafe "$1.onended" js_get_onended
  :: MediaStreamTrack -> (IO EventHandler)
foreign import javascript unsafe "$1.onended = $2" js_set_onended
  :: MediaStreamTrack -> (EventHandler -> (IO ()))
