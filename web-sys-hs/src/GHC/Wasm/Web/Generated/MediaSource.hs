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
module GHC.Wasm.Web.Generated.MediaSource (
        MediaSource, MediaSourceClass,
        js_fun_addSourceBuffer_DOMString_SourceBuffer,
        js_fun_removeSourceBuffer_SourceBuffer_undefined,
        js_fun_endOfStream_nullable_MediaSourceEndOfStreamError_undefined,
        js_fun_setLiveSeekableRange_double_double_undefined,
        js_fun_clearLiveSeekableRange__undefined, js_get_sourceBuffers,
        js_get_activeSourceBuffers, js_get_readyState, js_get_duration,
        js_set_duration, js_get_onsourceopen, js_set_onsourceopen,
        js_get_onsourceended, js_set_onsourceended, js_get_onsourceclose,
        js_set_onsourceclose,
        js_static_MediaSource_isTypeSupported_DOMString_boolean
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.MediaSource.Core
import GHC.Wasm.Web.Generated.MediaSourceEndOfStreamError.Core
import GHC.Wasm.Web.Generated.MediaSourceReadyState.Core
import GHC.Wasm.Web.Generated.SourceBuffer.Core
import GHC.Wasm.Web.Generated.SourceBufferList.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.addSourceBuffer($2)" js_fun_addSourceBuffer_DOMString_SourceBuffer
  :: MediaSource -> (DOMString -> (IO SourceBuffer))
foreign import javascript unsafe "$1.removeSourceBuffer($2)" js_fun_removeSourceBuffer_SourceBuffer_undefined
  :: MediaSource -> (SourceBuffer -> (IO ()))
foreign import javascript unsafe "$1.endOfStream($2)" js_fun_endOfStream_nullable_MediaSourceEndOfStreamError_undefined
  :: MediaSource
     -> (Nullable MediaSourceEndOfStreamErrorClass -> (IO ()))
foreign import javascript unsafe "$1.setLiveSeekableRange($2,$3)" js_fun_setLiveSeekableRange_double_double_undefined
  :: MediaSource -> (Double -> (Double -> (IO ())))
foreign import javascript unsafe "$1.clearLiveSeekableRange()" js_fun_clearLiveSeekableRange__undefined
  :: MediaSource -> (IO ())
foreign import javascript unsafe "$1.sourceBuffers" js_get_sourceBuffers
  :: MediaSource -> (IO SourceBufferList)
foreign import javascript unsafe "$1.activeSourceBuffers" js_get_activeSourceBuffers
  :: MediaSource -> (IO SourceBufferList)
foreign import javascript unsafe "$1.readyState" js_get_readyState
  :: MediaSource -> (IO MediaSourceReadyState)
foreign import javascript unsafe "$1.duration" js_get_duration
  :: MediaSource -> (IO Double)
foreign import javascript unsafe "$1.duration = $2" js_set_duration
  :: MediaSource -> (Double -> (IO ()))
foreign import javascript unsafe "$1.onsourceopen" js_get_onsourceopen
  :: MediaSource -> (IO EventHandler)
foreign import javascript unsafe "$1.onsourceopen = $2" js_set_onsourceopen
  :: MediaSource -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onsourceended" js_get_onsourceended
  :: MediaSource -> (IO EventHandler)
foreign import javascript unsafe "$1.onsourceended = $2" js_set_onsourceended
  :: MediaSource -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onsourceclose" js_get_onsourceclose
  :: MediaSource -> (IO EventHandler)
foreign import javascript unsafe "$1.onsourceclose = $2" js_set_onsourceclose
  :: MediaSource -> (EventHandler -> (IO ()))
foreign import javascript unsafe "MediaSource.isTypeSupported($1)" js_static_MediaSource_isTypeSupported_DOMString_boolean
  :: DOMString -> (IO Bool)
