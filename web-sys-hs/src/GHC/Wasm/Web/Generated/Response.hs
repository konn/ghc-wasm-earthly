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
module GHC.Wasm.Web.Generated.Response (
        Response, ResponseClass, js_cons_Response, js_fun_clone__Response,
        js_fun_cloneUnfiltered__Response,
        js_fun_arrayBuffer__Promise_ArrayBuffer, js_fun_blob__Promise_Blob,
        js_fun_formData__Promise_FormData, js_fun_json__Promise_JSON,
        js_fun_text__Promise_USVString, js_get_type, js_get_url,
        js_get_redirected, js_get_status, js_get_ok, js_get_statusText,
        js_get_headers, js_get_bodyUsed, js_get_body
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Blob.Core
import GHC.Wasm.Web.Generated.BodyInit.Core
import GHC.Wasm.Web.Generated.FormData.Core
import GHC.Wasm.Web.Generated.Headers.Core
import GHC.Wasm.Web.Generated.JSON.Core
import GHC.Wasm.Web.Generated.ReadableStream.Core
import GHC.Wasm.Web.Generated.Response.Core
import GHC.Wasm.Web.Generated.ResponseInit.Core
import GHC.Wasm.Web.Generated.ResponseType.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new Response($1,$2)" js_cons_Response
  :: Nullable (NullableClass BodyInitClass)
     -> (Nullable ResponseInitClass -> (IO Response))
foreign import javascript unsafe "$1.clone()" js_fun_clone__Response
  :: Response -> (IO Response)
foreign import javascript unsafe "$1.cloneUnfiltered()" js_fun_cloneUnfiltered__Response
  :: Response -> (IO Response)
foreign import javascript safe "$1.arrayBuffer()" js_fun_arrayBuffer__Promise_ArrayBuffer
  :: Response -> (IO (Promise ArrayBufferClass))
foreign import javascript safe "$1.blob()" js_fun_blob__Promise_Blob
  :: Response -> (IO (Promise BlobClass))
foreign import javascript safe "$1.formData()" js_fun_formData__Promise_FormData
  :: Response -> (IO (Promise FormDataClass))
foreign import javascript safe "$1.json()" js_fun_json__Promise_JSON
  :: Response -> (IO (Promise JSONClass))
foreign import javascript safe "$1.text()" js_fun_text__Promise_USVString
  :: Response -> (IO (Promise USVStringClass))
foreign import javascript unsafe "$1.type" js_get_type
  :: Response -> (IO ResponseType)
foreign import javascript unsafe "$1.url" js_get_url
  :: Response -> (IO USVString)
foreign import javascript unsafe "$1.redirected" js_get_redirected
  :: Response -> (IO Bool)
foreign import javascript unsafe "$1.status" js_get_status
  :: Response -> (IO Word16)
foreign import javascript unsafe "$1.ok" js_get_ok
  :: Response -> (IO Bool)
foreign import javascript unsafe "$1.statusText" js_get_statusText
  :: Response -> (IO JSByteString)
foreign import javascript unsafe "$1.headers" js_get_headers
  :: Response -> (IO Headers)
foreign import javascript unsafe "$1.bodyUsed" js_get_bodyUsed
  :: Response -> (IO Bool)
foreign import javascript unsafe "$1.body" js_get_body
  :: Response -> (IO (Nullable ReadableStreamClass))
