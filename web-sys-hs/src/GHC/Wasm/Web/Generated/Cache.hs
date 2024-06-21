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
module GHC.Wasm.Web.Generated.Cache (
        Cache, CacheClass,
        js_fun_match_RequestInfo_nullable_CacheQueryOptions_Promise_Response,
        js_fun_matchAll_nullable_RequestInfo_nullable_CacheQueryOptions_Promise_sequence_Response,
        js_fun_add_RequestInfo_Promise_undefined,
        js_fun_addAll_sequence_RequestInfo_Promise_undefined,
        js_fun_put_RequestInfo_Response_Promise_undefined,
        js_fun_delete_RequestInfo_nullable_CacheQueryOptions_Promise_boolean,
        js_fun_keys_nullable_RequestInfo_nullable_CacheQueryOptions_Promise_sequence_Request
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Cache.Core
import GHC.Wasm.Web.Generated.CacheQueryOptions.Core
import GHC.Wasm.Web.Generated.Request.Core
import GHC.Wasm.Web.Generated.RequestInfo.Core
import GHC.Wasm.Web.Generated.Response.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.match($2,$3)" js_fun_match_RequestInfo_nullable_CacheQueryOptions_Promise_Response
  :: Cache
     -> (RequestInfo
         -> (Nullable CacheQueryOptionsClass
             -> (IO (Promise ResponseClass))))
foreign import javascript safe "$1.matchAll($2,$3)" js_fun_matchAll_nullable_RequestInfo_nullable_CacheQueryOptions_Promise_sequence_Response
  :: Cache
     -> (Nullable RequestInfoClass
         -> (Nullable CacheQueryOptionsClass
             -> (IO (Promise (SequenceClass ResponseClass)))))
foreign import javascript safe "$1.add($2)" js_fun_add_RequestInfo_Promise_undefined
  :: Cache -> (RequestInfo -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.addAll($2)" js_fun_addAll_sequence_RequestInfo_Promise_undefined
  :: Cache
     -> (Sequence RequestInfoClass -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.put($2,$3)" js_fun_put_RequestInfo_Response_Promise_undefined
  :: Cache
     -> (RequestInfo -> (Response -> (IO (Promise UndefinedClass))))
foreign import javascript safe "$1.delete($2,$3)" js_fun_delete_RequestInfo_nullable_CacheQueryOptions_Promise_boolean
  :: Cache
     -> (RequestInfo
         -> (Nullable CacheQueryOptionsClass
             -> (IO (Promise (JSPrimClass Bool)))))
foreign import javascript safe "$1.keys($2,$3)" js_fun_keys_nullable_RequestInfo_nullable_CacheQueryOptions_Promise_sequence_Request
  :: Cache
     -> (Nullable RequestInfoClass
         -> (Nullable CacheQueryOptionsClass
             -> (IO (Promise (SequenceClass RequestClass)))))
