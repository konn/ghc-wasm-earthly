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
module GHC.Wasm.Web.Generated.PushSubscriptionInit.Core (
        PushSubscriptionInitFields, PushSubscriptionInitClass,
        PushSubscriptionInit, ReifiedPushSubscriptionInit
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type PushSubscriptionInitFields =
    '[ '("appServerKey",
         NullableClass (NullableClass BufferSourceClass)),
       '("authSecret", NullableClass (NullableClass ArrayBufferClass)),
       '("endpoint", USVStringClass),
       '("p256dhKey", NullableClass (NullableClass ArrayBufferClass)),
       '("scope", USVStringClass)]
type PushSubscriptionInitClass =
    JSDictionaryClass PushSubscriptionInitFields
type PushSubscriptionInit = JSObject PushSubscriptionInitClass
type ReifiedPushSubscriptionInit =
    ReifiedDictionary PushSubscriptionInitFields
