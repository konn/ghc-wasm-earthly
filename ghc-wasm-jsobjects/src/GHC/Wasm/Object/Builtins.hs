{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Builtins (
  module GHC.Wasm.Object.Core,
  PromiseClass,
  Promise,
  upcastPromise,
  await,
  module GHC.Wasm.Object.Builtins.Array,
  module GHC.Wasm.Object.Builtins.BigInt,
  module GHC.Wasm.Object.Builtins.Buffer,
  module GHC.Wasm.Object.Builtins.Dictionary,
  module GHC.Wasm.Object.Builtins.Iterable,
  module GHC.Wasm.Object.Builtins.Record,
  module GHC.Wasm.Object.Builtins.Sequence,
  module GHC.Wasm.Object.Builtins.String,
) where

import GHC.Exts (UnliftedType)
import GHC.Wasm.Object.Builtins.Array
import GHC.Wasm.Object.Builtins.BigInt
import GHC.Wasm.Object.Builtins.Buffer
import GHC.Wasm.Object.Builtins.Dictionary
import GHC.Wasm.Object.Builtins.Iterable
import GHC.Wasm.Object.Builtins.Record
import GHC.Wasm.Object.Builtins.Sequence
import GHC.Wasm.Object.Builtins.String
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
