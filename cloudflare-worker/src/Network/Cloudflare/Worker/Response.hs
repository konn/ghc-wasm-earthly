{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Network.Cloudflare.Worker.Response (
  WorkerResponseClass,
  WorkerResponse,
  WorkerResponseInit,
  ReifiedWorkerResponseInit,
  WorkerResponseInitClass,
  getBody,
  setBody,
  getHeaders,
  setHeaders,
  getUrl,
  getStatus,
  setStatus,
  getStatusText,
  setStatusText,
) where

import Data.Text qualified as T
import Data.Word
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Prim
import GHC.Wasm.Web.Generated.Headers
import GHC.Wasm.Web.Generated.ReadableStream
import GHC.Wasm.Web.Generated.Response
import GHC.Wasm.Web.Generated.WebSocket
import System.IO.Unsafe (unsafePerformIO)

type data WorkerResponseClass :: Prototype

type instance SuperclassOf WorkerResponseClass = 'Just ResponseClass

type WorkerResponse = JSObject WorkerResponseClass

getBody :: WorkerResponse -> IO (Nullable ReadableStreamClass)
getBody = js_get_body . upcast

setBody :: WorkerResponse -> Nullable ReadableStreamClass -> IO ()
setBody = js_set_body . upcast

foreign import javascript unsafe "$1.body = $2"
  js_set_body :: WorkerResponse -> Nullable ReadableStreamClass -> IO ()

getBodyUsed :: WorkerResponse -> IO Bool
getBodyUsed = js_get_bodyUsed . upcast

getHeaders :: WorkerResponse -> IO Headers
getHeaders = js_get_headers . upcast

setHeaders :: WorkerResponse -> Headers -> IO ()
setHeaders = js_set_headers

foreign import javascript unsafe "$1.body = $2"
  js_set_headers :: WorkerResponse -> Headers -> IO ()

getStatus :: WorkerResponse -> IO Word16
getStatus = js_get_status . upcast

setStatus :: WorkerResponse -> Word16 -> IO ()
setStatus = js_set_status

foreign import javascript unsafe "$1.status = $2"
  js_set_status :: WorkerResponse -> Word16 -> IO ()

getStatusText :: WorkerResponse -> IO T.Text
getStatusText = fmap (T.pack . fromJSString . convertToJSString) . js_get_statusText . upcast

setStatusText :: WorkerResponse -> T.Text -> IO ()
setStatusText res = js_set_status_text res . toJSString . T.unpack

foreign import javascript unsafe "$1.statusText = $2"
  js_set_status_text :: WorkerResponse -> JSString -> IO ()

getUrl :: WorkerResponse -> T.Text
getUrl = T.pack . fromJSString . convertToJSString . unsafePerformIO . js_get_url . upcast

type WorkerResponseInitFields =
  '[ '("status", JSPrimClass Word16)
   , '("statusText", JSByteStringClass)
   , '("headers", UnionClass '[HeadersClass, JSByteStringClass])
   , '("encodeBody", JSByteStringClass)
   , '("websocket", NullableClass WebSocketClass)
   , '("cf", NullableClass AnyClass)
   ]

type WorkerResponseInitClass = DictionaryClass WorkerResponseInitFields

type WorkerResponseInit = Dictionary WorkerResponseInitFields

type ReifiedWorkerResponseInit = ReifiedDictionary WorkerResponseInitFields
