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
module GHC.Wasm.Web.Generated.HTMLMediaElement (
        HTMLMediaElement, HTMLMediaElementClass,
        js_const_HTMLMediaElement_NETWORK_EMPTY,
        js_const_HTMLMediaElement_NETWORK_IDLE,
        js_const_HTMLMediaElement_NETWORK_LOADING,
        js_const_HTMLMediaElement_NETWORK_NO_SOURCE,
        js_const_HTMLMediaElement_HAVE_NOTHING,
        js_const_HTMLMediaElement_HAVE_METADATA,
        js_const_HTMLMediaElement_HAVE_CURRENT_DATA,
        js_const_HTMLMediaElement_HAVE_FUTURE_DATA,
        js_const_HTMLMediaElement_HAVE_ENOUGH_DATA, js_fun_load__undefined,
        js_fun_canPlayType_DOMString_DOMString,
        js_fun_fastSeek_double_undefined, js_fun_play__Promise_undefined,
        js_fun_pause__undefined,
        js_fun_addTextTrack_TextTrackKind_nullable_DOMString_nullable_DOMString_TextTrack,
        js_fun_setMediaKeys_nullable_MediaKeys_Promise_undefined,
        js_fun_seekToNextFrame__Promise_undefined,
        js_fun_setVisible_boolean_undefined,
        js_fun_hasSuspendTaint__boolean, js_get_error, js_get_src,
        js_set_src, js_get_currentSrc, js_get_srcObject, js_set_srcObject,
        js_get_crossOrigin, js_set_crossOrigin, js_get_networkState,
        js_get_preload, js_set_preload, js_get_buffered, js_get_readyState,
        js_get_seeking, js_get_currentTime, js_set_currentTime,
        js_get_duration, js_get_isEncrypted, js_get_paused,
        js_get_defaultPlaybackRate, js_set_defaultPlaybackRate,
        js_get_playbackRate, js_set_playbackRate, js_get_played,
        js_get_seekable, js_get_ended, js_get_autoplay, js_set_autoplay,
        js_get_loop, js_set_loop, js_get_controls, js_set_controls,
        js_get_volume, js_set_volume, js_get_muted, js_set_muted,
        js_get_defaultMuted, js_set_defaultMuted, js_get_audioTracks,
        js_get_videoTracks, js_get_textTracks, js_get_mediaKeys,
        js_get_onencrypted, js_set_onencrypted, js_get_onwaitingforkey,
        js_set_onwaitingforkey
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AudioTrackList.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.HTMLElement.Core
import GHC.Wasm.Web.Generated.HTMLMediaElement.Core
import GHC.Wasm.Web.Generated.MediaError.Core
import GHC.Wasm.Web.Generated.MediaKeys.Core
import GHC.Wasm.Web.Generated.MediaStream.Core
import GHC.Wasm.Web.Generated.TextTrack.Core
import GHC.Wasm.Web.Generated.TextTrackKind.Core
import GHC.Wasm.Web.Generated.TextTrackList.Core
import GHC.Wasm.Web.Generated.TimeRanges.Core
import GHC.Wasm.Web.Generated.VideoTrackList.Core
import GHC.Wasm.Web.Types
js_const_HTMLMediaElement_NETWORK_EMPTY :: Word16
js_const_HTMLMediaElement_NETWORK_EMPTY = 0
js_const_HTMLMediaElement_NETWORK_IDLE :: Word16
js_const_HTMLMediaElement_NETWORK_IDLE = 1
js_const_HTMLMediaElement_NETWORK_LOADING :: Word16
js_const_HTMLMediaElement_NETWORK_LOADING = 2
js_const_HTMLMediaElement_NETWORK_NO_SOURCE :: Word16
js_const_HTMLMediaElement_NETWORK_NO_SOURCE = 3
js_const_HTMLMediaElement_HAVE_NOTHING :: Word16
js_const_HTMLMediaElement_HAVE_NOTHING = 0
js_const_HTMLMediaElement_HAVE_METADATA :: Word16
js_const_HTMLMediaElement_HAVE_METADATA = 1
js_const_HTMLMediaElement_HAVE_CURRENT_DATA :: Word16
js_const_HTMLMediaElement_HAVE_CURRENT_DATA = 2
js_const_HTMLMediaElement_HAVE_FUTURE_DATA :: Word16
js_const_HTMLMediaElement_HAVE_FUTURE_DATA = 3
js_const_HTMLMediaElement_HAVE_ENOUGH_DATA :: Word16
js_const_HTMLMediaElement_HAVE_ENOUGH_DATA = 4
foreign import javascript unsafe "$1.load()" js_fun_load__undefined
  :: HTMLMediaElement -> (IO ())
foreign import javascript unsafe "$1.canPlayType($2)" js_fun_canPlayType_DOMString_DOMString
  :: HTMLMediaElement -> (DOMString -> (IO DOMString))
foreign import javascript unsafe "$1.fastSeek($2)" js_fun_fastSeek_double_undefined
  :: HTMLMediaElement -> (Double -> (IO ()))
foreign import javascript safe "$1.play()" js_fun_play__Promise_undefined
  :: HTMLMediaElement -> (IO (Promise UndefinedClass))
foreign import javascript unsafe "$1.pause()" js_fun_pause__undefined
  :: HTMLMediaElement -> (IO ())
foreign import javascript unsafe "$1.addTextTrack($2,$3,$4)" js_fun_addTextTrack_TextTrackKind_nullable_DOMString_nullable_DOMString_TextTrack
  :: HTMLMediaElement
     -> (TextTrackKind
         -> (Nullable DOMStringClass
             -> (Nullable DOMStringClass -> (IO TextTrack))))
foreign import javascript safe "$1.setMediaKeys($2)" js_fun_setMediaKeys_nullable_MediaKeys_Promise_undefined
  :: HTMLMediaElement
     -> (Nullable MediaKeysClass -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.seekToNextFrame()" js_fun_seekToNextFrame__Promise_undefined
  :: HTMLMediaElement -> (IO (Promise UndefinedClass))
foreign import javascript unsafe "$1.setVisible($2)" js_fun_setVisible_boolean_undefined
  :: HTMLMediaElement -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.hasSuspendTaint()" js_fun_hasSuspendTaint__boolean
  :: HTMLMediaElement -> (IO Bool)
foreign import javascript unsafe "$1.error" js_get_error
  :: HTMLMediaElement -> (IO (Nullable MediaErrorClass))
foreign import javascript unsafe "$1.src" js_get_src
  :: HTMLMediaElement -> (IO DOMString)
foreign import javascript unsafe "$1.src = $2" js_set_src
  :: HTMLMediaElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.currentSrc" js_get_currentSrc
  :: HTMLMediaElement -> (IO DOMString)
foreign import javascript unsafe "$1.srcObject" js_get_srcObject
  :: HTMLMediaElement -> (IO (Nullable MediaStreamClass))
foreign import javascript unsafe "$1.srcObject = $2" js_set_srcObject
  :: HTMLMediaElement -> (Nullable MediaStreamClass -> (IO ()))
foreign import javascript unsafe "$1.crossOrigin" js_get_crossOrigin
  :: HTMLMediaElement -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.crossOrigin = $2" js_set_crossOrigin
  :: HTMLMediaElement -> (Nullable DOMStringClass -> (IO ()))
foreign import javascript unsafe "$1.networkState" js_get_networkState
  :: HTMLMediaElement -> (IO Word16)
foreign import javascript unsafe "$1.preload" js_get_preload
  :: HTMLMediaElement -> (IO DOMString)
foreign import javascript unsafe "$1.preload = $2" js_set_preload
  :: HTMLMediaElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.buffered" js_get_buffered
  :: HTMLMediaElement -> (IO TimeRanges)
foreign import javascript unsafe "$1.readyState" js_get_readyState
  :: HTMLMediaElement -> (IO Word16)
foreign import javascript unsafe "$1.seeking" js_get_seeking
  :: HTMLMediaElement -> (IO Bool)
foreign import javascript unsafe "$1.currentTime" js_get_currentTime
  :: HTMLMediaElement -> (IO Double)
foreign import javascript unsafe "$1.currentTime = $2" js_set_currentTime
  :: HTMLMediaElement -> (Double -> (IO ()))
foreign import javascript unsafe "$1.duration" js_get_duration
  :: HTMLMediaElement -> (IO Double)
foreign import javascript unsafe "$1.isEncrypted" js_get_isEncrypted
  :: HTMLMediaElement -> (IO Bool)
foreign import javascript unsafe "$1.paused" js_get_paused
  :: HTMLMediaElement -> (IO Bool)
foreign import javascript unsafe "$1.defaultPlaybackRate" js_get_defaultPlaybackRate
  :: HTMLMediaElement -> (IO Double)
foreign import javascript unsafe "$1.defaultPlaybackRate = $2" js_set_defaultPlaybackRate
  :: HTMLMediaElement -> (Double -> (IO ()))
foreign import javascript unsafe "$1.playbackRate" js_get_playbackRate
  :: HTMLMediaElement -> (IO Double)
foreign import javascript unsafe "$1.playbackRate = $2" js_set_playbackRate
  :: HTMLMediaElement -> (Double -> (IO ()))
foreign import javascript unsafe "$1.played" js_get_played
  :: HTMLMediaElement -> (IO TimeRanges)
foreign import javascript unsafe "$1.seekable" js_get_seekable
  :: HTMLMediaElement -> (IO TimeRanges)
foreign import javascript unsafe "$1.ended" js_get_ended
  :: HTMLMediaElement -> (IO Bool)
foreign import javascript unsafe "$1.autoplay" js_get_autoplay
  :: HTMLMediaElement -> (IO Bool)
foreign import javascript unsafe "$1.autoplay = $2" js_set_autoplay
  :: HTMLMediaElement -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.loop" js_get_loop
  :: HTMLMediaElement -> (IO Bool)
foreign import javascript unsafe "$1.loop = $2" js_set_loop
  :: HTMLMediaElement -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.controls" js_get_controls
  :: HTMLMediaElement -> (IO Bool)
foreign import javascript unsafe "$1.controls = $2" js_set_controls
  :: HTMLMediaElement -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.volume" js_get_volume
  :: HTMLMediaElement -> (IO Double)
foreign import javascript unsafe "$1.volume = $2" js_set_volume
  :: HTMLMediaElement -> (Double -> (IO ()))
foreign import javascript unsafe "$1.muted" js_get_muted
  :: HTMLMediaElement -> (IO Bool)
foreign import javascript unsafe "$1.muted = $2" js_set_muted
  :: HTMLMediaElement -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.defaultMuted" js_get_defaultMuted
  :: HTMLMediaElement -> (IO Bool)
foreign import javascript unsafe "$1.defaultMuted = $2" js_set_defaultMuted
  :: HTMLMediaElement -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.audioTracks" js_get_audioTracks
  :: HTMLMediaElement -> (IO AudioTrackList)
foreign import javascript unsafe "$1.videoTracks" js_get_videoTracks
  :: HTMLMediaElement -> (IO VideoTrackList)
foreign import javascript unsafe "$1.textTracks" js_get_textTracks
  :: HTMLMediaElement -> (IO (Nullable TextTrackListClass))
foreign import javascript unsafe "$1.mediaKeys" js_get_mediaKeys
  :: HTMLMediaElement -> (IO (Nullable MediaKeysClass))
foreign import javascript unsafe "$1.onencrypted" js_get_onencrypted
  :: HTMLMediaElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onencrypted = $2" js_set_onencrypted
  :: HTMLMediaElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwaitingforkey" js_get_onwaitingforkey
  :: HTMLMediaElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onwaitingforkey = $2" js_set_onwaitingforkey
  :: HTMLMediaElement -> (EventHandler -> (IO ()))
