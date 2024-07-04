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
module GHC.Wasm.Web.Generated.MediaStream (
        MediaStream, MediaStreamClass, js_cons_MediaStream_MediaStream,
        js_cons_MediaStream_sequence_MediaStreamTrack,
        js_fun_getAudioTracks__sequence_AudioStreamTrack,
        js_fun_getVideoTracks__sequence_VideoStreamTrack,
        js_fun_getTracks__sequence_MediaStreamTrack,
        js_fun_getTrackById_DOMString_nullable_MediaStreamTrack,
        js_fun_addTrack_MediaStreamTrack_undefined,
        js_fun_removeTrack_MediaStreamTrack_undefined,
        js_fun_clone__MediaStream, js_fun_assignId_DOMString_undefined,
        js_get_id, js_get_active, js_get_onaddtrack, js_set_onaddtrack,
        js_get_onremovetrack, js_set_onremovetrack, js_get_currentTime,
        js_static_MediaStream_countUnderlyingStreams__Promise_long
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AudioStreamTrack.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.MediaStream.Core
import GHC.Wasm.Web.Generated.MediaStreamTrack.Core
import GHC.Wasm.Web.Generated.VideoStreamTrack.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new MediaStream($1)" js_cons_MediaStream_MediaStream
  :: MediaStream -> (IO MediaStream)
foreign import javascript unsafe "new MediaStream($1)" js_cons_MediaStream_sequence_MediaStreamTrack
  :: Sequence MediaStreamTrackClass -> (IO MediaStream)
foreign import javascript unsafe "$1.getAudioTracks()" js_fun_getAudioTracks__sequence_AudioStreamTrack
  :: MediaStream -> (IO (Sequence AudioStreamTrackClass))
foreign import javascript unsafe "$1.getVideoTracks()" js_fun_getVideoTracks__sequence_VideoStreamTrack
  :: MediaStream -> (IO (Sequence VideoStreamTrackClass))
foreign import javascript unsafe "$1.getTracks()" js_fun_getTracks__sequence_MediaStreamTrack
  :: MediaStream -> (IO (Sequence MediaStreamTrackClass))
foreign import javascript unsafe "$1.getTrackById($2)" js_fun_getTrackById_DOMString_nullable_MediaStreamTrack
  :: MediaStream
     -> (DOMString -> (IO (Nullable MediaStreamTrackClass)))
foreign import javascript unsafe "$1.addTrack($2)" js_fun_addTrack_MediaStreamTrack_undefined
  :: MediaStream -> (MediaStreamTrack -> (IO ()))
foreign import javascript unsafe "$1.removeTrack($2)" js_fun_removeTrack_MediaStreamTrack_undefined
  :: MediaStream -> (MediaStreamTrack -> (IO ()))
foreign import javascript unsafe "$1.clone()" js_fun_clone__MediaStream
  :: MediaStream -> (IO MediaStream)
foreign import javascript unsafe "$1.assignId($2)" js_fun_assignId_DOMString_undefined
  :: MediaStream -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.id" js_get_id
  :: MediaStream -> (IO DOMString)
foreign import javascript unsafe "$1.active" js_get_active
  :: MediaStream -> (IO Bool)
foreign import javascript unsafe "$1.onaddtrack" js_get_onaddtrack
  :: MediaStream -> (IO EventHandler)
foreign import javascript unsafe "$1.onaddtrack = $2" js_set_onaddtrack
  :: MediaStream -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onremovetrack" js_get_onremovetrack
  :: MediaStream -> (IO EventHandler)
foreign import javascript unsafe "$1.onremovetrack = $2" js_set_onremovetrack
  :: MediaStream -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.currentTime" js_get_currentTime
  :: MediaStream -> (IO Double)
foreign import javascript safe "MediaStream.countUnderlyingStreams()" js_static_MediaStream_countUnderlyingStreams__Promise_long
  :: IO (Promise (JSPrimClass Int32))
