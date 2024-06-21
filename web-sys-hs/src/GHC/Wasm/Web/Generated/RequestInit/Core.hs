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
module GHC.Wasm.Web.Generated.RequestInit.Core (
        RequestInitFields, RequestInitClass, RequestInit,
        ReifiedRequestInit
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AbortSignal.Core
import GHC.Wasm.Web.Generated.BodyInit.Core
import GHC.Wasm.Web.Generated.HeadersInit.Core
import GHC.Wasm.Web.Generated.ObserverCallback.Core
import GHC.Wasm.Web.Generated.ReferrerPolicy.Core
import GHC.Wasm.Web.Generated.RequestCache.Core
import GHC.Wasm.Web.Generated.RequestCredentials.Core
import GHC.Wasm.Web.Generated.RequestMode.Core
import GHC.Wasm.Web.Generated.RequestRedirect.Core
import GHC.Wasm.Web.Types
type RequestInitFields =
    '[ '("body", NullableClass (NullableClass BodyInitClass)),
       '("cache", NullableClass RequestCacheClass),
       '("credentials", NullableClass RequestCredentialsClass),
       '("headers", NullableClass HeadersInitClass),
       '("integrity", NullableClass DOMStringClass),
       '("method", NullableClass JSByteStringClass),
       '("mode", NullableClass RequestModeClass),
       '("observe", NullableClass ObserverCallbackClass),
       '("redirect", NullableClass RequestRedirectClass),
       '("referrer", NullableClass USVStringClass),
       '("referrerPolicy", NullableClass ReferrerPolicyClass),
       '("signal", NullableClass (NullableClass AbortSignalClass))]
type RequestInitClass = JSDictionaryClass RequestInitFields
type RequestInit = JSObject RequestInitClass
type ReifiedRequestInit = ReifiedDictionary RequestInitFields
