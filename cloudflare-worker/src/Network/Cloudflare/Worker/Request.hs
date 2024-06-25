{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Network.Cloudflare.Worker.Request (
  WorkerRequest,
  newRequest,
  getUrl,
  readBody,
  getBody,
  getBodyUsed,
  getHeaders,
  getHeaders',
  getMethod,
  getRedirect,
  getCloudflare,
  getCloudflareJSON,
  decodeWorkerRequest,
  WorkerRequestInitField,
  WorkerRequestClass,
  WorkerRequestInitRecord,
  WorkerRequestInit,
  WorkerRequestInitClass,
  RequestInitCfProperties,
  RequestInitCfRecord,
  RequestInitCfPropertiesClass,
  RequestInitCfPropertiesFields,
  MinifyInit,
  MinifyInitClass,
  MinifyInitFields,
  WorkerIncomingRequestCf,
  WorkerIncomingRequestCfClass,
  WorkerIncomingRequestCfFields,
) where

import Control.Monad
import Data.Aeson qualified as J
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Word
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Prim
import GHC.Wasm.Web.Generated.FormData
import GHC.Wasm.Web.Generated.Headers
import GHC.Wasm.Web.Generated.ReadableStream
import GHC.Wasm.Web.Generated.Request
import GHC.Wasm.Web.Generated.RequestRedirect (RequestRedirect)
import GHC.Wasm.Web.Generated.URLSearchParams
import GHC.Wasm.Web.JSON
import GHC.Wasm.Web.ReadableStream
import Streaming.ByteString qualified as Q
import Streaming.Prelude qualified as S
import System.IO.Unsafe (unsafePerformIO)

type data WorkerRequestClass :: Prototype

type instance SuperclassOf WorkerRequestClass = 'Just RequestClass

type WorkerRequest = JSObject WorkerRequestClass

newRequest :: Maybe T.Text -> Maybe WorkerRequestInit -> IO WorkerRequest
newRequest mbody minit =
  js_new_request
    (toNullable $ toUSVString . toJSString . T.unpack <$> mbody)
    (toNullable minit)

foreign import javascript unsafe "new Request($1, $2)"
  js_new_request :: Nullable USVStringClass -> Nullable WorkerRequestInitClass -> IO WorkerRequest

decodeWorkerRequest :: WorkerRequest -> IO (Maybe J.Value)
decodeWorkerRequest = decodeJSON <=< await <=< js_cf_req_to_json

foreign import javascript safe "$1.json()"
  js_cf_req_to_json :: WorkerRequest -> IO (Promise JSONClass)

type BodyLikeClass = UnionClass '[JSByteStringClass, ReadableStreamClass, FormDataClass, URLSearchParamsClass]

type WorkerRequestInitField =
  '[ '("cf", NullableClass RequestInitCfPropertiesClass)
   , '("method", NullableClass JSByteStringClass)
   , '("headers", NullableClass HeadersClass)
   , '("body", NullableClass BodyLikeClass)
   , '("redirect", NullableClass JSByteStringClass)
   ]

type WorkerRequestInitClass = JSDictionaryClass WorkerRequestInitField

type WorkerRequestInit = JSObject WorkerRequestInitClass

type WorkerRequestInitRecord = ReifiedDictionary WorkerRequestInitField

type RequestInitCfPropertiesFields =
  '[ '("apps", NullableClass (JSPrimClass Bool))
   , '("cacheEverything", NullableClass (JSPrimClass Bool))
   , '("cacheKey", NullableClass USVStringClass)
   , '("cacheTags", NullableClass (SequenceClass JSByteStringClass))
   , '("cacheTtl", NullableClass (JSPrimClass Word32))
   , '("cacheTtlByStatus", NullableClass (JSRecordClass USVStringClass (JSPrimClass Word32)))
   , '("image", NullableClass (NullableClass AnyClass))
   , '("minify", NullableClass MinifyInitClass)
   , '("mirage", NullableClass (JSPrimClass Bool))
   , '("polish", NullableClass JSByteStringClass)
   , '("resolveOverride", NullableClass JSByteStringClass)
   , '("scrapeShield", NullableClass (JSPrimClass Bool))
   , '("webp", NullableClass (JSPrimClass Bool))
   ]

type RequestInitCfRecord = ReifiedDictionary RequestInitCfPropertiesFields

type RequestInitCfPropertiesClass = JSDictionaryClass RequestInitCfPropertiesFields

type RequestInitCfProperties = JSObject RequestInitCfPropertiesClass

type MinifyInitFields =
  '[ '("javascript", NullableClass (JSPrimClass Bool))
   , '("css", NullableClass (JSPrimClass Bool))
   , '("html", NullableClass (JSPrimClass Bool))
   ]

type MinifyInitClass = JSDictionaryClass MinifyInitFields

type MinifyInit = JSDictionary MinifyInitFields

getUrl :: WorkerRequest -> JSString
getUrl = unsafePerformIO . fmap convertToJSString . js_get_url . upcast

type WorkerIncomingRequestCfFields =
  '[ '("asn", JSPrimClass Word32)
   , '("asOrganization", USVStringClass)
   , '("botManagement", NullableClass AnyClass)
   , '("clientAcceptEncoding", NullableClass USVStringClass)
   , '("colo", USVStringClass)
   , '("country", NullableClass USVStringClass)
   , '("isEUCountry", NullableClass USVStringClass)
   , '("httpProtocol", USVStringClass)
   , '("requestPriority", NullableClass USVStringClass)
   , '("tlsCipher", USVStringClass)
   , '("tlsClientAuth", NullableClass AnyClass)
   , '("tlsClientHelloLength", USVStringClass)
   , '("tlsClientRandom", USVStringClass)
   , '("tlsVersion", USVStringClass)
   , '("city", NullableClass USVStringClass)
   , '("continent", NullableClass USVStringClass)
   , '("latitude", NullableClass USVStringClass)
   , '("longitude", NullableClass USVStringClass)
   , '("postalCode", NullableClass USVStringClass)
   , '("metroCode", NullableClass USVStringClass)
   , '("region", NullableClass USVStringClass)
   , '("regionCode", NullableClass USVStringClass)
   , '("timezone", USVStringClass)
   ]

type WorkerIncomingRequestCfClass = JSDictionaryClass WorkerIncomingRequestCfFields

type WorkerIncomingRequestCf = JSDictionary WorkerIncomingRequestCfFields

getCloudflareJSON :: WorkerRequest -> IO (Maybe J.Value)
getCloudflareJSON = decodeJSON . unsafeCast . js_get_cf

getCloudflare :: WorkerRequest -> WorkerIncomingRequestCf
getCloudflare = js_get_cf

foreign import javascript unsafe "$1.cf"
  js_get_cf :: WorkerRequest -> WorkerIncomingRequestCf

getBody :: WorkerRequest -> Nullable ReadableStreamClass
getBody = unsafePerformIO . js_get_body . upcast

readBody :: WorkerRequest -> Maybe (Q.ByteStream IO ())
readBody = fmap fromReadableStream . fromNullable . getBody

getBodyUsed :: WorkerRequest -> Bool
getBodyUsed = unsafePerformIO . js_get_bodyUsed . upcast

getHeaders :: WorkerRequest -> [(BS.ByteString, BS.ByteString)]
getHeaders =
  unsafePerformIO
    . ( S.toList_
          . S.mapM (bitraverse toHaskellByteString toHaskellByteString)
          . fromPairIterable
          <=< js_iter_Headers_ByteString_ByteString
            . getHeaders'
      )

getHeaders' :: WorkerRequest -> Headers
getHeaders' = unsafePerformIO . js_get_headers . upcast

getMethod :: WorkerRequest -> JSByteString
getMethod = unsafePerformIO . js_get_method . upcast

getRedirect :: WorkerRequest -> RequestRedirect
getRedirect = unsafePerformIO . js_get_redirect . upcast
