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
module GHC.Wasm.Web.Generated.CacheQueryOptions.Core (
        CacheQueryOptionsFields, CacheQueryOptionsClass, CacheQueryOptions,
        ReifiedCacheQueryOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type CacheQueryOptionsFields =
    '[ '("cacheName", NullableClass DOMStringClass),
       '("ignoreMethod", NullableClass (JSPrimClass Bool)),
       '("ignoreSearch", NullableClass (JSPrimClass Bool)),
       '("ignoreVary", NullableClass (JSPrimClass Bool))]
type CacheQueryOptionsClass =
    JSDictionaryClass CacheQueryOptionsFields
type CacheQueryOptions = JSObject CacheQueryOptionsClass
type ReifiedCacheQueryOptions =
    ReifiedDictionary CacheQueryOptionsFields
