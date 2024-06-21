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
module GHC.Wasm.Web.Generated.Animation (
        Animation, AnimationClass, js_cons_Animation,
        js_fun_cancel__undefined, js_fun_finish__undefined,
        js_fun_play__undefined, js_fun_pause__undefined,
        js_fun_updatePlaybackRate_double_undefined,
        js_fun_reverse__undefined, js_get_id, js_set_id, js_get_effect,
        js_set_effect, js_get_timeline, js_set_timeline, js_get_startTime,
        js_set_startTime, js_get_currentTime, js_set_currentTime,
        js_get_playbackRate, js_set_playbackRate, js_get_playState,
        js_get_pending, js_get_ready, js_get_finished, js_get_onfinish,
        js_set_onfinish, js_get_oncancel, js_set_oncancel,
        js_get_isRunningOnCompositor
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Animation.Core
import GHC.Wasm.Web.Generated.AnimationEffect.Core
import GHC.Wasm.Web.Generated.AnimationPlayState.Core
import GHC.Wasm.Web.Generated.AnimationTimeline.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new Animation($1,$2)" js_cons_Animation
  :: Nullable (NullableClass AnimationEffectClass)
     -> (Nullable (NullableClass AnimationTimelineClass)
         -> (IO Animation))
foreign import javascript unsafe "$1.cancel()" js_fun_cancel__undefined
  :: Animation -> (IO ())
foreign import javascript unsafe "$1.finish()" js_fun_finish__undefined
  :: Animation -> (IO ())
foreign import javascript unsafe "$1.play()" js_fun_play__undefined
  :: Animation -> (IO ())
foreign import javascript unsafe "$1.pause()" js_fun_pause__undefined
  :: Animation -> (IO ())
foreign import javascript unsafe "$1.updatePlaybackRate($2)" js_fun_updatePlaybackRate_double_undefined
  :: Animation -> (Double -> (IO ()))
foreign import javascript unsafe "$1.reverse()" js_fun_reverse__undefined
  :: Animation -> (IO ())
foreign import javascript unsafe "$1.id" js_get_id
  :: Animation -> (IO DOMString)
foreign import javascript unsafe "$1.id = $2" js_set_id
  :: Animation -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.effect" js_get_effect
  :: Animation -> (IO (Nullable AnimationEffectClass))
foreign import javascript unsafe "$1.effect = $2" js_set_effect
  :: Animation -> (Nullable AnimationEffectClass -> (IO ()))
foreign import javascript unsafe "$1.timeline" js_get_timeline
  :: Animation -> (IO (Nullable AnimationTimelineClass))
foreign import javascript unsafe "$1.timeline = $2" js_set_timeline
  :: Animation -> (Nullable AnimationTimelineClass -> (IO ()))
foreign import javascript unsafe "$1.startTime" js_get_startTime
  :: Animation -> (IO (Nullable (JSPrimClass Double)))
foreign import javascript unsafe "$1.startTime = $2" js_set_startTime
  :: Animation -> (Nullable (JSPrimClass Double) -> (IO ()))
foreign import javascript unsafe "$1.currentTime" js_get_currentTime
  :: Animation -> (IO (Nullable (JSPrimClass Double)))
foreign import javascript unsafe "$1.currentTime = $2" js_set_currentTime
  :: Animation -> (Nullable (JSPrimClass Double) -> (IO ()))
foreign import javascript unsafe "$1.playbackRate" js_get_playbackRate
  :: Animation -> (IO Double)
foreign import javascript unsafe "$1.playbackRate = $2" js_set_playbackRate
  :: Animation -> (Double -> (IO ()))
foreign import javascript unsafe "$1.playState" js_get_playState
  :: Animation -> (IO AnimationPlayState)
foreign import javascript unsafe "$1.pending" js_get_pending
  :: Animation -> (IO Bool)
foreign import javascript unsafe "$1.ready" js_get_ready
  :: Animation -> (IO (Promise AnimationClass))
foreign import javascript unsafe "$1.finished" js_get_finished
  :: Animation -> (IO (Promise AnimationClass))
foreign import javascript unsafe "$1.onfinish" js_get_onfinish
  :: Animation -> (IO EventHandler)
foreign import javascript unsafe "$1.onfinish = $2" js_set_onfinish
  :: Animation -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncancel" js_get_oncancel
  :: Animation -> (IO EventHandler)
foreign import javascript unsafe "$1.oncancel = $2" js_set_oncancel
  :: Animation -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.isRunningOnCompositor" js_get_isRunningOnCompositor
  :: Animation -> (IO Bool)
