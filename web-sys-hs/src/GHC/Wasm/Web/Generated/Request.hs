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
module GHC.Wasm.Web.Generated.Request (
        Request, RequestClass, js_cons_Request, js_fun_clone__Request,
        js_fun_overrideContentPolicyType_nsContentPolicyType_undefined,
        js_fun_arrayBuffer__Promise_ArrayBuffer, js_fun_blob__Promise_Blob,
        js_fun_formData__Promise_FormData, js_fun_json__Promise_JSON,
        js_fun_text__Promise_USVString, js_get_method, js_get_url,
        js_get_headers, js_get_destination, js_get_referrer,
        js_get_referrerPolicy, js_get_mode, js_get_credentials,
        js_get_cache, js_get_redirect, js_get_integrity, js_get_signal,
        js_get_bodyUsed, js_get_body
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AbortSignal.Core
import GHC.Wasm.Web.Generated.Blob.Core
import GHC.Wasm.Web.Generated.FormData.Core
import GHC.Wasm.Web.Generated.Headers.Core
import GHC.Wasm.Web.Generated.JSON.Core
import GHC.Wasm.Web.Generated.NsContentPolicyType.Core
import GHC.Wasm.Web.Generated.ReadableStream.Core
import GHC.Wasm.Web.Generated.ReferrerPolicy.Core
import GHC.Wasm.Web.Generated.Request.Core
import GHC.Wasm.Web.Generated.RequestCache.Core
import GHC.Wasm.Web.Generated.RequestCredentials.Core
import GHC.Wasm.Web.Generated.RequestDestination.Core
import GHC.Wasm.Web.Generated.RequestInfo.Core
import GHC.Wasm.Web.Generated.RequestInit.Core
import GHC.Wasm.Web.Generated.RequestMode.Core
import GHC.Wasm.Web.Generated.RequestRedirect.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new Request($1,$2)" js_cons_Request
  :: RequestInfo -> (Nullable RequestInitClass -> (IO Request))
foreign import javascript unsafe "$1.clone()" js_fun_clone__Request
  :: Request -> (IO Request)
foreign import javascript unsafe "$1.overrideContentPolicyType($2)" js_fun_overrideContentPolicyType_nsContentPolicyType_undefined
  :: Request -> (NsContentPolicyType -> (IO ()))
foreign import javascript safe "$1.arrayBuffer()" js_fun_arrayBuffer__Promise_ArrayBuffer
  :: Request -> (IO (Promise ArrayBufferClass))
foreign import javascript safe "$1.blob()" js_fun_blob__Promise_Blob
  :: Request -> (IO (Promise BlobClass))
foreign import javascript safe "$1.formData()" js_fun_formData__Promise_FormData
  :: Request -> (IO (Promise FormDataClass))
foreign import javascript safe "$1.json()" js_fun_json__Promise_JSON
  :: Request -> (IO (Promise JSONClass))
foreign import javascript safe "$1.text()" js_fun_text__Promise_USVString
  :: Request -> (IO (Promise USVStringClass))
foreign import javascript unsafe "$1.method" js_get_method
  :: Request -> (IO JSByteString)
foreign import javascript unsafe "$1.url" js_get_url
  :: Request -> (IO USVString)
foreign import javascript unsafe "$1.headers" js_get_headers
  :: Request -> (IO Headers)
foreign import javascript unsafe "$1.destination" js_get_destination
  :: Request -> (IO RequestDestination)
foreign import javascript unsafe "$1.referrer" js_get_referrer
  :: Request -> (IO USVString)
foreign import javascript unsafe "$1.referrerPolicy" js_get_referrerPolicy
  :: Request -> (IO ReferrerPolicy)
foreign import javascript unsafe "$1.mode" js_get_mode
  :: Request -> (IO RequestMode)
foreign import javascript unsafe "$1.credentials" js_get_credentials
  :: Request -> (IO RequestCredentials)
foreign import javascript unsafe "$1.cache" js_get_cache
  :: Request -> (IO RequestCache)
foreign import javascript unsafe "$1.redirect" js_get_redirect
  :: Request -> (IO RequestRedirect)
foreign import javascript unsafe "$1.integrity" js_get_integrity
  :: Request -> (IO DOMString)
foreign import javascript unsafe "$1.signal" js_get_signal
  :: Request -> (IO AbortSignal)
foreign import javascript unsafe "$1.bodyUsed" js_get_bodyUsed
  :: Request -> (IO Bool)
foreign import javascript unsafe "$1.body" js_get_body
  :: Request -> (IO (Nullable ReadableStreamClass))
