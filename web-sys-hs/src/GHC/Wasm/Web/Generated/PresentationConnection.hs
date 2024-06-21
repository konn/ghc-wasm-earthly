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
module GHC.Wasm.Web.Generated.PresentationConnection (
        PresentationConnection, PresentationConnectionClass,
        js_fun_send_DOMString_undefined, js_fun_send_Blob_undefined,
        js_fun_send_ArrayBuffer_undefined,
        js_fun_send_ArrayBufferView_undefined, js_fun_close__undefined,
        js_fun_terminate__undefined, js_get_id, js_get_url, js_get_state,
        js_get_onconnect, js_set_onconnect, js_get_onclose, js_set_onclose,
        js_get_onterminate, js_set_onterminate, js_get_binaryType,
        js_set_binaryType, js_get_onmessage, js_set_onmessage
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Blob.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.PresentationConnection.Core
import GHC.Wasm.Web.Generated.PresentationConnectionBinaryType.Core
import GHC.Wasm.Web.Generated.PresentationConnectionState.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.send($2)" js_fun_send_DOMString_undefined
  :: PresentationConnection -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.send($2)" js_fun_send_Blob_undefined
  :: PresentationConnection -> (Blob -> (IO ()))
foreign import javascript unsafe "$1.send($2)" js_fun_send_ArrayBuffer_undefined
  :: PresentationConnection -> (ArrayBuffer -> (IO ()))
foreign import javascript unsafe "$1.send($2)" js_fun_send_ArrayBufferView_undefined
  :: PresentationConnection -> (ArrayBufferView -> (IO ()))
foreign import javascript unsafe "$1.close()" js_fun_close__undefined
  :: PresentationConnection -> (IO ())
foreign import javascript unsafe "$1.terminate()" js_fun_terminate__undefined
  :: PresentationConnection -> (IO ())
foreign import javascript unsafe "$1.id" js_get_id
  :: PresentationConnection -> (IO DOMString)
foreign import javascript unsafe "$1.url" js_get_url
  :: PresentationConnection -> (IO DOMString)
foreign import javascript unsafe "$1.state" js_get_state
  :: PresentationConnection -> (IO PresentationConnectionState)
foreign import javascript unsafe "$1.onconnect" js_get_onconnect
  :: PresentationConnection -> (IO EventHandler)
foreign import javascript unsafe "$1.onconnect = $2" js_set_onconnect
  :: PresentationConnection -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onclose" js_get_onclose
  :: PresentationConnection -> (IO EventHandler)
foreign import javascript unsafe "$1.onclose = $2" js_set_onclose
  :: PresentationConnection -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onterminate" js_get_onterminate
  :: PresentationConnection -> (IO EventHandler)
foreign import javascript unsafe "$1.onterminate = $2" js_set_onterminate
  :: PresentationConnection -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.binaryType" js_get_binaryType
  :: PresentationConnection -> (IO PresentationConnectionBinaryType)
foreign import javascript unsafe "$1.binaryType = $2" js_set_binaryType
  :: PresentationConnection
     -> (PresentationConnectionBinaryType -> (IO ()))
foreign import javascript unsafe "$1.onmessage" js_get_onmessage
  :: PresentationConnection -> (IO EventHandler)
foreign import javascript unsafe "$1.onmessage = $2" js_set_onmessage
  :: PresentationConnection -> (EventHandler -> (IO ()))
