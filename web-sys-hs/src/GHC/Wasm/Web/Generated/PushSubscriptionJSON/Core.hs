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
module GHC.Wasm.Web.Generated.PushSubscriptionJSON.Core (
        PushSubscriptionJSONFields, PushSubscriptionJSONClass,
        PushSubscriptionJSON, ReifiedPushSubscriptionJSON
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.PushSubscriptionKeys.Core
import GHC.Wasm.Web.Types
type PushSubscriptionJSONFields =
    '[ '("endpoint", NullableClass USVStringClass),
       '("keys", NullableClass PushSubscriptionKeysClass)]
type PushSubscriptionJSONClass =
    JSDictionaryClass PushSubscriptionJSONFields
type PushSubscriptionJSON = JSObject PushSubscriptionJSONClass
type ReifiedPushSubscriptionJSON =
    ReifiedDictionary PushSubscriptionJSONFields
