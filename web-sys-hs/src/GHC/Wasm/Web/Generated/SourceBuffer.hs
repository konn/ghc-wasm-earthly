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
module GHC.Wasm.Web.Generated.SourceBuffer (
        SourceBuffer, SourceBufferClass,
        js_fun_appendBuffer_ArrayBuffer_undefined,
        js_fun_appendBuffer_ArrayBufferView_undefined,
        js_fun_appendBufferAsync_ArrayBuffer_Promise_undefined,
        js_fun_appendBufferAsync_ArrayBufferView_Promise_undefined,
        js_fun_abort__undefined, js_fun_remove_double_double_undefined,
        js_fun_removeAsync_double_double_Promise_undefined,
        js_fun_changeType_DOMString_undefined, js_get_mode, js_set_mode,
        js_get_updating, js_get_buffered, js_get_timestampOffset,
        js_set_timestampOffset, js_get_audioTracks, js_get_videoTracks,
        js_get_textTracks, js_get_appendWindowStart,
        js_set_appendWindowStart, js_get_appendWindowEnd,
        js_set_appendWindowEnd, js_get_onupdatestart, js_set_onupdatestart,
        js_get_onupdate, js_set_onupdate, js_get_onupdateend,
        js_set_onupdateend, js_get_onerror, js_set_onerror, js_get_onabort,
        js_set_onabort
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AudioTrackList.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.SourceBuffer.Core
import GHC.Wasm.Web.Generated.SourceBufferAppendMode.Core
import GHC.Wasm.Web.Generated.TextTrackList.Core
import GHC.Wasm.Web.Generated.TimeRanges.Core
import GHC.Wasm.Web.Generated.VideoTrackList.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.appendBuffer($2)" js_fun_appendBuffer_ArrayBuffer_undefined
  :: SourceBuffer -> (ArrayBuffer -> (IO ()))
foreign import javascript unsafe "$1.appendBuffer($2)" js_fun_appendBuffer_ArrayBufferView_undefined
  :: SourceBuffer -> (ArrayBufferView -> (IO ()))
foreign import javascript safe "$1.appendBufferAsync($2)" js_fun_appendBufferAsync_ArrayBuffer_Promise_undefined
  :: SourceBuffer -> (ArrayBuffer -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.appendBufferAsync($2)" js_fun_appendBufferAsync_ArrayBufferView_Promise_undefined
  :: SourceBuffer
     -> (ArrayBufferView -> (IO (Promise UndefinedClass)))
foreign import javascript unsafe "$1.abort()" js_fun_abort__undefined
  :: SourceBuffer -> (IO ())
foreign import javascript unsafe "$1.remove($2,$3)" js_fun_remove_double_double_undefined
  :: SourceBuffer -> (Double -> (Double -> (IO ())))
foreign import javascript safe "$1.removeAsync($2,$3)" js_fun_removeAsync_double_double_Promise_undefined
  :: SourceBuffer
     -> (Double -> (Double -> (IO (Promise UndefinedClass))))
foreign import javascript unsafe "$1.changeType($2)" js_fun_changeType_DOMString_undefined
  :: SourceBuffer -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.mode" js_get_mode
  :: SourceBuffer -> (IO SourceBufferAppendMode)
foreign import javascript unsafe "$1.mode = $2" js_set_mode
  :: SourceBuffer -> (SourceBufferAppendMode -> (IO ()))
foreign import javascript unsafe "$1.updating" js_get_updating
  :: SourceBuffer -> (IO Bool)
foreign import javascript unsafe "$1.buffered" js_get_buffered
  :: SourceBuffer -> (IO TimeRanges)
foreign import javascript unsafe "$1.timestampOffset" js_get_timestampOffset
  :: SourceBuffer -> (IO Double)
foreign import javascript unsafe "$1.timestampOffset = $2" js_set_timestampOffset
  :: SourceBuffer -> (Double -> (IO ()))
foreign import javascript unsafe "$1.audioTracks" js_get_audioTracks
  :: SourceBuffer -> (IO AudioTrackList)
foreign import javascript unsafe "$1.videoTracks" js_get_videoTracks
  :: SourceBuffer -> (IO VideoTrackList)
foreign import javascript unsafe "$1.textTracks" js_get_textTracks
  :: SourceBuffer -> (IO TextTrackList)
foreign import javascript unsafe "$1.appendWindowStart" js_get_appendWindowStart
  :: SourceBuffer -> (IO Double)
foreign import javascript unsafe "$1.appendWindowStart = $2" js_set_appendWindowStart
  :: SourceBuffer -> (Double -> (IO ()))
foreign import javascript unsafe "$1.appendWindowEnd" js_get_appendWindowEnd
  :: SourceBuffer -> (IO Double)
foreign import javascript unsafe "$1.appendWindowEnd = $2" js_set_appendWindowEnd
  :: SourceBuffer -> (Double -> (IO ()))
foreign import javascript unsafe "$1.onupdatestart" js_get_onupdatestart
  :: SourceBuffer -> (IO EventHandler)
foreign import javascript unsafe "$1.onupdatestart = $2" js_set_onupdatestart
  :: SourceBuffer -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onupdate" js_get_onupdate
  :: SourceBuffer -> (IO EventHandler)
foreign import javascript unsafe "$1.onupdate = $2" js_set_onupdate
  :: SourceBuffer -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onupdateend" js_get_onupdateend
  :: SourceBuffer -> (IO EventHandler)
foreign import javascript unsafe "$1.onupdateend = $2" js_set_onupdateend
  :: SourceBuffer -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onerror" js_get_onerror
  :: SourceBuffer -> (IO EventHandler)
foreign import javascript unsafe "$1.onerror = $2" js_set_onerror
  :: SourceBuffer -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onabort" js_get_onabort
  :: SourceBuffer -> (IO EventHandler)
foreign import javascript unsafe "$1.onabort = $2" js_set_onabort
  :: SourceBuffer -> (EventHandler -> (IO ()))
