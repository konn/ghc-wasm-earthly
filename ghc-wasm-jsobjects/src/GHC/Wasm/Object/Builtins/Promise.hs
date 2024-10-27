{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Builtins.Promise (
  PromiseClass,
  Promise,
  Promised (..),
  newPromise,
  upcastPromise,
  await,
  await',
  awaitWith,
  awaitWithM,
  deferWithM,
  deferWith,
  jsPromise,
) where

import Control.Concurrent.Async (Async, async)
import Control.Monad ((<=<))
import GHC.Exts (UnliftedType)
import GHC.Generics (Generic, Generic1)
import GHC.Wasm.Object.Core

type PromiseClass :: Prototype -> UnliftedType
type data PromiseClass c

type instance SuperclassOf (PromiseClass c) = 'Nothing

-- | JavaScript Promise object.
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

-- | Javascript 'Promise' with the extractor of Haskell value
data Promised js a = Promised !(JSObject js -> IO a) !(Promise js)
  deriving (Generic, Generic1, Functor)

await' :: Promised js a -> IO a
await' (Promised f p) = f =<< await p

jsPromise :: Promised js a -> Promise js
jsPromise (Promised _ p) = p

foreign import javascript safe "await $1"
  js_await :: Promise a -> IO (JSObject a)

foreign import javascript unsafe "new Promise((resolve) => {resolve($1)})"
  newPromise :: JSObject a -> IO (Promise a)
