{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Builtins.Promise (
  PromiseClass,
  Promise,
  upcastPromise,
  await,
) where

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

foreign import javascript safe "await $1"
  js_await :: Promise a -> IO (JSObject a)
