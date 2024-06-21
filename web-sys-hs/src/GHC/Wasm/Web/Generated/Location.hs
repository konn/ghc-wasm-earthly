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
module GHC.Wasm.Web.Generated.Location (
        Location, LocationClass, js_fun_assign_USVString_undefined,
        js_fun_replace_USVString_undefined,
        js_fun_reload_nullable_boolean_undefined, js_get_href, js_set_href,
        js_get_origin, js_get_protocol, js_set_protocol, js_get_host,
        js_set_host, js_get_hostname, js_set_hostname, js_get_port,
        js_set_port, js_get_pathname, js_set_pathname, js_get_search,
        js_set_search, js_get_hash, js_set_hash
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Location.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.assign($2)" js_fun_assign_USVString_undefined
  :: Location -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.replace($2)" js_fun_replace_USVString_undefined
  :: Location -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.reload($2)" js_fun_reload_nullable_boolean_undefined
  :: Location -> (Nullable (JSPrimClass Bool) -> (IO ()))
foreign import javascript unsafe "$1.href" js_get_href
  :: Location -> (IO USVString)
foreign import javascript unsafe "$1.href = $2" js_set_href
  :: Location -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.origin" js_get_origin
  :: Location -> (IO USVString)
foreign import javascript unsafe "$1.protocol" js_get_protocol
  :: Location -> (IO USVString)
foreign import javascript unsafe "$1.protocol = $2" js_set_protocol
  :: Location -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.host" js_get_host
  :: Location -> (IO USVString)
foreign import javascript unsafe "$1.host = $2" js_set_host
  :: Location -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.hostname" js_get_hostname
  :: Location -> (IO USVString)
foreign import javascript unsafe "$1.hostname = $2" js_set_hostname
  :: Location -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.port" js_get_port
  :: Location -> (IO USVString)
foreign import javascript unsafe "$1.port = $2" js_set_port
  :: Location -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.pathname" js_get_pathname
  :: Location -> (IO USVString)
foreign import javascript unsafe "$1.pathname = $2" js_set_pathname
  :: Location -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.search" js_get_search
  :: Location -> (IO USVString)
foreign import javascript unsafe "$1.search = $2" js_set_search
  :: Location -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.hash" js_get_hash
  :: Location -> (IO USVString)
foreign import javascript unsafe "$1.hash = $2" js_set_hash
  :: Location -> (USVString -> (IO ()))
