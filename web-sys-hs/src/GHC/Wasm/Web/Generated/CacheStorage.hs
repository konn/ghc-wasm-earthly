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
module GHC.Wasm.Web.Generated.CacheStorage (
        CacheStorage, CacheStorageClass,
        js_fun_match_RequestInfo_nullable_CacheQueryOptions_Promise_Response,
        js_fun_has_DOMString_Promise_boolean,
        js_fun_open_DOMString_Promise_Cache,
        js_fun_delete_DOMString_Promise_boolean,
        js_fun_keys__Promise_sequence_DOMString
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Cache.Core
import GHC.Wasm.Web.Generated.CacheQueryOptions.Core
import GHC.Wasm.Web.Generated.CacheStorage.Core
import GHC.Wasm.Web.Generated.RequestInfo.Core
import GHC.Wasm.Web.Generated.Response.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.match($2,$3)" js_fun_match_RequestInfo_nullable_CacheQueryOptions_Promise_Response
  :: CacheStorage
     -> (RequestInfo
         -> (Nullable CacheQueryOptionsClass
             -> (IO (Promise ResponseClass))))
foreign import javascript safe "$1.has($2)" js_fun_has_DOMString_Promise_boolean
  :: CacheStorage -> (DOMString -> (IO (Promise (JSPrimClass Bool))))
foreign import javascript safe "$1.open($2)" js_fun_open_DOMString_Promise_Cache
  :: CacheStorage -> (DOMString -> (IO (Promise CacheClass)))
foreign import javascript safe "$1.delete($2)" js_fun_delete_DOMString_Promise_boolean
  :: CacheStorage -> (DOMString -> (IO (Promise (JSPrimClass Bool))))
foreign import javascript safe "$1.keys()" js_fun_keys__Promise_sequence_DOMString
  :: CacheStorage -> (IO (Promise (SequenceClass DOMStringClass)))
