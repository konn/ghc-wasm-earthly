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
module GHC.Wasm.Web.Generated.WebSocket (
        WebSocket, WebSocketClass, js_cons_WebSocket_DOMString,
        js_cons_WebSocket_DOMString_DOMString,
        js_cons_WebSocket_DOMString_sequence_DOMString,
        js_const_WebSocket_CONNECTING, js_const_WebSocket_OPEN,
        js_const_WebSocket_CLOSING, js_const_WebSocket_CLOSED,
        js_fun_close_nullable_short_nullable_DOMString_undefined,
        js_fun_send_DOMString_undefined, js_fun_send_Blob_undefined,
        js_fun_send_ArrayBuffer_undefined,
        js_fun_send_ArrayBufferView_undefined, js_get_url,
        js_get_readyState, js_get_bufferedAmount, js_get_onopen,
        js_set_onopen, js_get_onerror, js_set_onerror, js_get_onclose,
        js_set_onclose, js_get_extensions, js_get_protocol,
        js_get_onmessage, js_set_onmessage, js_get_binaryType,
        js_set_binaryType,
        js_static_WebSocket_createServerWebSocket_DOMString_sequence_DOMString_nsITransportProvider_DOMString_WebSocket
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.BinaryType.Core
import GHC.Wasm.Web.Generated.Blob.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.NsITransportProvider.Core
import GHC.Wasm.Web.Generated.WebSocket.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new WebSocket($1)" js_cons_WebSocket_DOMString
  :: DOMString -> (IO WebSocket)
foreign import javascript unsafe "new WebSocket($1,$2)" js_cons_WebSocket_DOMString_DOMString
  :: DOMString -> (DOMString -> (IO WebSocket))
foreign import javascript unsafe "new WebSocket($1,$2)" js_cons_WebSocket_DOMString_sequence_DOMString
  :: DOMString -> (Sequence DOMStringClass -> (IO WebSocket))
js_const_WebSocket_CONNECTING :: Word16
js_const_WebSocket_CONNECTING = 0
js_const_WebSocket_OPEN :: Word16
js_const_WebSocket_OPEN = 1
js_const_WebSocket_CLOSING :: Word16
js_const_WebSocket_CLOSING = 2
js_const_WebSocket_CLOSED :: Word16
js_const_WebSocket_CLOSED = 3
foreign import javascript unsafe "$1.close($2,$3)" js_fun_close_nullable_short_nullable_DOMString_undefined
  :: WebSocket
     -> (Nullable (JSPrimClass Word16)
         -> (Nullable DOMStringClass -> (IO ())))
foreign import javascript unsafe "$1.send($2)" js_fun_send_DOMString_undefined
  :: WebSocket -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.send($2)" js_fun_send_Blob_undefined
  :: WebSocket -> (Blob -> (IO ()))
foreign import javascript unsafe "$1.send($2)" js_fun_send_ArrayBuffer_undefined
  :: WebSocket -> (ArrayBuffer -> (IO ()))
foreign import javascript unsafe "$1.send($2)" js_fun_send_ArrayBufferView_undefined
  :: WebSocket -> (ArrayBufferView -> (IO ()))
foreign import javascript unsafe "$1.url" js_get_url
  :: WebSocket -> (IO DOMString)
foreign import javascript unsafe "$1.readyState" js_get_readyState
  :: WebSocket -> (IO Word16)
foreign import javascript unsafe "$1.bufferedAmount" js_get_bufferedAmount
  :: WebSocket -> (IO Word32)
foreign import javascript unsafe "$1.onopen" js_get_onopen
  :: WebSocket -> (IO EventHandler)
foreign import javascript unsafe "$1.onopen = $2" js_set_onopen
  :: WebSocket -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onerror" js_get_onerror
  :: WebSocket -> (IO EventHandler)
foreign import javascript unsafe "$1.onerror = $2" js_set_onerror
  :: WebSocket -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onclose" js_get_onclose
  :: WebSocket -> (IO EventHandler)
foreign import javascript unsafe "$1.onclose = $2" js_set_onclose
  :: WebSocket -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.extensions" js_get_extensions
  :: WebSocket -> (IO DOMString)
foreign import javascript unsafe "$1.protocol" js_get_protocol
  :: WebSocket -> (IO DOMString)
foreign import javascript unsafe "$1.onmessage" js_get_onmessage
  :: WebSocket -> (IO EventHandler)
foreign import javascript unsafe "$1.onmessage = $2" js_set_onmessage
  :: WebSocket -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.binaryType" js_get_binaryType
  :: WebSocket -> (IO BinaryType)
foreign import javascript unsafe "$1.binaryType = $2" js_set_binaryType
  :: WebSocket -> (BinaryType -> (IO ()))
foreign import javascript unsafe "WebSocket.createServerWebSocket($1,$2,$3,$4)" js_static_WebSocket_createServerWebSocket_DOMString_sequence_DOMString_nsITransportProvider_DOMString_WebSocket
  :: DOMString
     -> (Sequence DOMStringClass
         -> (NsITransportProvider -> (DOMString -> (IO WebSocket))))
