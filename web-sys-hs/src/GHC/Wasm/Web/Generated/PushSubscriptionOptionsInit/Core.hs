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
module GHC.Wasm.Web.Generated.PushSubscriptionOptionsInit.Core (
        PushSubscriptionOptionsInitFields,
        PushSubscriptionOptionsInitClass, PushSubscriptionOptionsInit,
        ReifiedPushSubscriptionOptionsInit
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type PushSubscriptionOptionsInitFields =
    '[ '("applicationServerKey",
         NullableClass (NullableClass (UnionClass '[BufferSourceClass,
                                                    DOMStringClass]))),
       '("userVisibleOnly", NullableClass (JSPrimClass Bool))]
type PushSubscriptionOptionsInitClass =
    JSDictionaryClass PushSubscriptionOptionsInitFields
type PushSubscriptionOptionsInit =
    JSObject PushSubscriptionOptionsInitClass
type ReifiedPushSubscriptionOptionsInit =
    ReifiedDictionary PushSubscriptionOptionsInitFields
