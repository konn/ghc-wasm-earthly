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
module GHC.Wasm.Web.Generated.PushSubscriptionKeys.Core (
        PushSubscriptionKeysFields, PushSubscriptionKeysClass,
        PushSubscriptionKeys, ReifiedPushSubscriptionKeys
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type PushSubscriptionKeysFields =
    '[ '("auth", NullableClass JSByteStringClass),
       '("p256dh", NullableClass JSByteStringClass)]
type PushSubscriptionKeysClass =
    JSDictionaryClass PushSubscriptionKeysFields
type PushSubscriptionKeys = JSObject PushSubscriptionKeysClass
type ReifiedPushSubscriptionKeys =
    ReifiedDictionary PushSubscriptionKeysFields
