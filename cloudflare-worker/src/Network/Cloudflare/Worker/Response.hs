{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Network.Cloudflare.Worker.Response (
  WorkerResponseClass,
  WorkerResponse,
  WorkerResponseInit,
  ReifiedWorkerResponseInit,
  WorkerResponseInitClass,
  SimpleResponseInit (..),
  WorkerResponseBody (..),
  fromWorkerResponseBody,
  ResponseBodyClass,
  ResponseBody,
  getBody,
  setBody,
  getBodyUsed,
  getHeaders,
  setHeaders,
  getUrl,
  getStatus,
  setStatus,
  getStatusText,
  setStatusText,
  newResponse',
  newResponse,
  toHeaders,
) where

import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Foldable (forM_)
import Data.Maybe (fromJust)
import Data.String
import Data.Text qualified as T
import Data.Word
import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Prim
import GHC.Wasm.Web.Generated.FormData (FormDataClass)
import GHC.Wasm.Web.Generated.Headers
import GHC.Wasm.Web.Generated.ReadableStream
import GHC.Wasm.Web.Generated.Response
import GHC.Wasm.Web.Generated.URLSearchParams (URLSearchParamsClass)
import GHC.Wasm.Web.Generated.WebSocket (WebSocketClass)
import System.IO.Unsafe (unsafePerformIO)
import Wasm.Prelude.Linear qualified as PL

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

foreign import javascript unsafe "$1.headers = $2"
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
getUrl = toText . unsafePerformIO . js_get_url . upcast

newResponse :: SimpleResponseInit -> IO WorkerResponse
newResponse resp = do
  headers <- toHeaders resp.headers
  empty <- emptyObject
  body <- mapM fromWorkerResponseBody resp.body
  newResponse' body . Just $
    newDictionary
      ( setPartialField "status" (toJSPrim resp.status)
          PL.. setPartialField
            "statusText"
            (fromJust $ toJSByteString $ toJSString $ BS8.unpack resp.statusText)
          PL.. setPartialField "headers" (inject headers)
          PL.. setPartialField "cf" empty
          PL.. setPartialField "encodeBody" (fromJust $ toJSByteString $ toJSString "automatic")
      )

fromWorkerResponseBody :: WorkerResponseBody -> IO ResponseBody
fromWorkerResponseBody (WorkerResponseLBS lbs) =
  useByteStringAsJSByteArray @Word8 (LBS.toStrict lbs) $ pure . inject
fromWorkerResponseBody (WorkerResponseBS bs) =
  useByteStringAsJSByteArray @Word8 bs $ pure . inject
fromWorkerResponseBody (WorkerResponseStream stream) = pure $ inject stream
fromWorkerResponseBody (WorkerResponseRaw stream) = pure stream

newResponse' :: Maybe ResponseBody -> Maybe WorkerResponseInit -> IO WorkerResponse
newResponse' mbody minit =
  js_new_response
    (toNullable mbody)
    (toNullable minit)

toHeaders :: [(CI BS.ByteString, BS.ByteString)] -> IO Headers
toHeaders dic = do
  hdrs0 <-
    js_cons_Headers . nonNull . inject
      =<< toJSRecord @JSByteStringClass @JSByteStringClass mempty
  forM_ dic \(k, v) -> do
    k' <- fromHaskellByteString $ CI.original k
    v' <- fromHaskellByteString v
    js_fun_append_ByteString_ByteString_undefined hdrs0 k' v'
  pure hdrs0

type ResponseBody = JSObject ResponseBodyClass

type ResponseBodyClass =
  UnionClass
    '[ BufferSourceClass
     , FormDataClass
     , ReadableStreamClass
     , URLSearchParamsClass
     , USVStringClass
     ]

foreign import javascript unsafe "new Response($1, $2)"
  js_new_response :: Nullable ResponseBodyClass -> Nullable WorkerResponseInitClass -> IO WorkerResponse

data WorkerResponseBody
  = WorkerResponseLBS !LBS.ByteString
  | WorkerResponseBS !BS.ByteString
  | WorkerResponseStream !ReadableStream
  | WorkerResponseRaw !ResponseBody
  deriving (Generic)

instance IsString WorkerResponseBody where
  fromString = WorkerResponseRaw . inject . fromText @USVStringClass . T.pack

data SimpleResponseInit = SimpleResponseInit
  { body :: !(Maybe WorkerResponseBody)
  , status :: !Word16
  , statusText :: !BS.ByteString
  , headers :: ![(CI BS.ByteString, BS.ByteString)]
  }
  deriving (Generic)

type WorkerResponseInitFields =
  '[ '("status", JSPrimClass Word16)
   , '("statusText", JSByteStringClass)
   , '("headers", UnionClass '[HeadersClass, JSByteStringClass])
   , '("encodeBody", JSByteStringClass)
   , '("websocket", NullableClass WebSocketClass)
   , '("cf", AnyClass)
   ]

type WorkerResponseInitClass = JSDictionaryClass WorkerResponseInitFields

type WorkerResponseInit = JSDictionary WorkerResponseInitFields

type ReifiedWorkerResponseInit = ReifiedDictionary WorkerResponseInitFields
