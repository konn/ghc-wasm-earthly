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
  module GHC.Wasm.Object.Builtins.String,
) where

import GHC.Exts (UnliftedType)
import GHC.Wasm.Object.Builtins.String
import GHC.Wasm.Object.Core

type PromiseClass :: UnliftedType -> UnliftedType
type data PromiseClass c

type instance SuperclassOf (PromiseClass c) = 'Nothing

type Promise v = JSObject (PromiseClass v)

upcastPromise :: (sub <: super) => Promise sub -> Promise super
upcastPromise = unsafeAsObject . unJSObject
