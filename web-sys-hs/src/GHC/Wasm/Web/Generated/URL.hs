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
module GHC.Wasm.Web.Generated.URL (
        URL, URLClass, js_cons_URL, js_fun_toJSON__USVString, js_get_href,
        js_set_href, js_get_origin, js_get_protocol, js_set_protocol,
        js_get_username, js_set_username, js_get_password, js_set_password,
        js_get_host, js_set_host, js_get_hostname, js_set_hostname,
        js_get_port, js_set_port, js_get_pathname, js_set_pathname,
        js_get_search, js_set_search, js_get_searchParams, js_get_hash,
        js_set_hash, js_static_URL_createObjectURL_Blob_DOMString,
        js_static_URL_revokeObjectURL_DOMString_undefined,
        js_static_URL_isValidURL_DOMString_boolean,
        js_static_URL_createObjectURL_MediaSource_DOMString
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Blob.Core
import GHC.Wasm.Web.Generated.MediaSource.Core
import GHC.Wasm.Web.Generated.URL.Core
import GHC.Wasm.Web.Generated.URLSearchParams.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new URL($1,$2)" js_cons_URL
  :: USVString -> (Nullable USVStringClass -> (IO URL))
foreign import javascript unsafe "$1.toJSON()" js_fun_toJSON__USVString
  :: URL -> (IO USVString)
foreign import javascript unsafe "$1.href" js_get_href
  :: URL -> (IO USVString)
foreign import javascript unsafe "$1.href = $2" js_set_href
  :: URL -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.origin" js_get_origin
  :: URL -> (IO USVString)
foreign import javascript unsafe "$1.protocol" js_get_protocol
  :: URL -> (IO USVString)
foreign import javascript unsafe "$1.protocol = $2" js_set_protocol
  :: URL -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.username" js_get_username
  :: URL -> (IO USVString)
foreign import javascript unsafe "$1.username = $2" js_set_username
  :: URL -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.password" js_get_password
  :: URL -> (IO USVString)
foreign import javascript unsafe "$1.password = $2" js_set_password
  :: URL -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.host" js_get_host
  :: URL -> (IO USVString)
foreign import javascript unsafe "$1.host = $2" js_set_host
  :: URL -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.hostname" js_get_hostname
  :: URL -> (IO USVString)
foreign import javascript unsafe "$1.hostname = $2" js_set_hostname
  :: URL -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.port" js_get_port
  :: URL -> (IO USVString)
foreign import javascript unsafe "$1.port = $2" js_set_port
  :: URL -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.pathname" js_get_pathname
  :: URL -> (IO USVString)
foreign import javascript unsafe "$1.pathname = $2" js_set_pathname
  :: URL -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.search" js_get_search
  :: URL -> (IO USVString)
foreign import javascript unsafe "$1.search = $2" js_set_search
  :: URL -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.searchParams" js_get_searchParams
  :: URL -> (IO URLSearchParams)
foreign import javascript unsafe "$1.hash" js_get_hash
  :: URL -> (IO USVString)
foreign import javascript unsafe "$1.hash = $2" js_set_hash
  :: URL -> (USVString -> (IO ()))
foreign import javascript unsafe "URL.createObjectURL($1)" js_static_URL_createObjectURL_Blob_DOMString
  :: Blob -> (IO DOMString)
foreign import javascript unsafe "URL.revokeObjectURL($1)" js_static_URL_revokeObjectURL_DOMString_undefined
  :: DOMString -> (IO ())
foreign import javascript unsafe "URL.isValidURL($1)" js_static_URL_isValidURL_DOMString_boolean
  :: DOMString -> (IO Bool)
foreign import javascript unsafe "URL.createObjectURL($1)" js_static_URL_createObjectURL_MediaSource_DOMString
  :: MediaSource -> (IO DOMString)
