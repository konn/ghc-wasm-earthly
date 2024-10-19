{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Builtins.Promise (
  PromiseClass,
  Promise,
  newPromise,
  upcastPromise,
  await,
  awaitWith,
  awaitWithM,
  deferWithM,
  deferWith,
) where

import Control.Concurrent.Async (Async, async)
import Control.Monad ((<=<))
import GHC.Exts (UnliftedType)
import GHC.Wasm.Object.Core

type PromiseClass :: Prototype -> UnliftedType
type data PromiseClass c

type instance SuperclassOf (PromiseClass c) = 'Nothing

type Promise v = JSObject (PromiseClass v)

upcastPromise :: (sub <: super) => Promise sub -> Promise super
upcastPromise = unsafeAsObject . unJSObject

await :: Promise a -> IO (JSObject a)
await = js_await

awaitWith :: (JSObject a -> b) -> Promise a -> IO b
awaitWith f = fmap f . await

awaitWithM :: (JSObject a -> IO b) -> Promise a -> IO b
awaitWithM f = f <=< await

deferWithM :: (JSObject a -> IO b) -> Promise a -> IO (Async b)
deferWithM f = async . f <=< await

deferWith :: (JSObject a -> b) -> Promise a -> IO (Async b)
deferWith f = async . fmap f . await

foreign import javascript safe "await $1"
  js_await :: Promise a -> IO (JSObject a)

foreign import javascript unsafe "new Promise((resolve) => {resolve($1)})"
  newPromise :: JSObject a -> IO (Promise a)
