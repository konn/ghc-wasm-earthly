{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Network.Cloudflare.Worker.Binding.Cache (
  WorkersCache,
  defaultCache,
  put,
  WorkersCacheOptions,
  WorkersCacheOptionsFields,
  WorkersCacheOptionsClass,
  match,
  delete,

  -- * General caches
  putTo,

  -- * Re-exports
) where

import Control.Monad ((<=<))
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Request (Request, RequestClass)
import GHC.Wasm.Web.Generated.Response (Response, ResponseClass)

type data WorkersCacheClass :: Prototype

type instance SuperclassOf WorkersCacheClass = 'Nothing

type WorkersCache = JSObject WorkersCacheClass

put :: Request -> Response -> IO (Promise UndefinedClass)
put = putTo defaultCache

foreign import javascript safe "$1.put($2, $3)"
  putTo :: WorkersCache -> Request -> Response -> IO (Promise UndefinedClass)

type WorkersCacheOptionsFields =
  '[ '("ignoreMethod", JSPrimClass Bool)]

type WorkersCacheOptionsClass = JSDictionaryClass WorkersCacheOptionsFields

type WorkersCacheOptions = JSObject WorkersCacheOptionsClass

match ::
  Union '[RequestClass, JSByteStringClass] ->
  Maybe WorkersCacheOptions ->
  IO (Promise (NullableClass ResponseClass))
match req = undefinedToNullPromise <=< rawMatch defaultCache req . toNullable

delete ::
  Union '[RequestClass, JSByteStringClass] ->
  Maybe WorkersCacheOptions ->
  IO (Promise (JSPrimClass Bool))
delete req = js_delete defaultCache req . toNullable

foreign import javascript safe "$1.match($2, $3)"
  rawMatch ::
    WorkersCache ->
    Union '[RequestClass, JSByteStringClass] ->
    Nullable WorkersCacheOptionsClass ->
    IO (Promise (UnionClass '[ResponseClass, UndefinedClass]))

foreign import javascript safe "$1.delete($2, $3)"
  js_delete ::
    WorkersCache ->
    Union '[RequestClass, JSByteStringClass] ->
    Nullable WorkersCacheOptionsClass ->
    IO (Promise (JSPrimClass Bool))

foreign import javascript safe "$1.then((value) => { if (!x) { return null; } else { return x; } })"
  undefinedToNullPromise :: Promise (UnionClass '[a, UndefinedClass]) -> IO (Promise (NullableClass a))

foreign import javascript unsafe "caches.default"
  defaultCache :: WorkersCache
