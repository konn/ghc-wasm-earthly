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
module GHC.Wasm.Web.Generated.PushSubscription (
        PushSubscription, PushSubscriptionClass,
        js_fun_getKey_PushEncryptionKeyName_nullable_ArrayBuffer,
        js_fun_unsubscribe__Promise_boolean,
        js_fun_toJSON__PushSubscriptionJSON, js_get_endpoint,
        js_get_options
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.PushEncryptionKeyName.Core
import GHC.Wasm.Web.Generated.PushSubscription.Core
import GHC.Wasm.Web.Generated.PushSubscriptionJSON.Core
import GHC.Wasm.Web.Generated.PushSubscriptionOptions.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getKey($2)" js_fun_getKey_PushEncryptionKeyName_nullable_ArrayBuffer
  :: PushSubscription
     -> (PushEncryptionKeyName -> (IO (Nullable ArrayBufferClass)))
foreign import javascript safe "$1.unsubscribe()" js_fun_unsubscribe__Promise_boolean
  :: PushSubscription -> (IO (Promise (JSPrimClass Bool)))
foreign import javascript unsafe "$1.toJSON()" js_fun_toJSON__PushSubscriptionJSON
  :: PushSubscription -> (IO PushSubscriptionJSON)
foreign import javascript unsafe "$1.endpoint" js_get_endpoint
  :: PushSubscription -> (IO USVString)
foreign import javascript unsafe "$1.options" js_get_options
  :: PushSubscription -> (IO PushSubscriptionOptions)
