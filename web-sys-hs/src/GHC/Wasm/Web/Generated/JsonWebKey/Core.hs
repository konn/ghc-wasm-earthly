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
module GHC.Wasm.Web.Generated.JsonWebKey.Core (
        JsonWebKeyFields, JsonWebKeyClass, JsonWebKey, ReifiedJsonWebKey
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.RsaOtherPrimesInfo.Core
import GHC.Wasm.Web.Types
type JsonWebKeyFields =
    '[ '("alg", NullableClass DOMStringClass),
       '("crv", NullableClass DOMStringClass),
       '("d", NullableClass DOMStringClass),
       '("dp", NullableClass DOMStringClass),
       '("dq", NullableClass DOMStringClass),
       '("e", NullableClass DOMStringClass),
       '("ext", NullableClass (JSPrimClass Bool)),
       '("k", NullableClass DOMStringClass),
       '("key_ops", NullableClass (SequenceClass DOMStringClass)),
       '("kty", DOMStringClass),
       '("n", NullableClass DOMStringClass),
       '("oth", NullableClass (SequenceClass RsaOtherPrimesInfoClass)),
       '("p", NullableClass DOMStringClass),
       '("q", NullableClass DOMStringClass),
       '("qi", NullableClass DOMStringClass),
       '("use", NullableClass DOMStringClass),
       '("x", NullableClass DOMStringClass),
       '("y", NullableClass DOMStringClass)]
type JsonWebKeyClass = JSDictionaryClass JsonWebKeyFields
type JsonWebKey = JSObject JsonWebKeyClass
type ReifiedJsonWebKey = ReifiedDictionary JsonWebKeyFields
