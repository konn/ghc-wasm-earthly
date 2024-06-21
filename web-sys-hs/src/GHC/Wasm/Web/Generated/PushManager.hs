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
module GHC.Wasm.Web.Generated.PushManager (
        PushManager, PushManagerClass,
        js_fun_subscribe_nullable_PushSubscriptionOptionsInit_Promise_PushSubscription,
        js_fun_getSubscription__Promise_nullable_PushSubscription,
        js_fun_permissionState_nullable_PushSubscriptionOptionsInit_Promise_PushPermissionState
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.PushManager.Core
import GHC.Wasm.Web.Generated.PushPermissionState.Core
import GHC.Wasm.Web.Generated.PushSubscription.Core
import GHC.Wasm.Web.Generated.PushSubscriptionOptionsInit.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.subscribe($2)" js_fun_subscribe_nullable_PushSubscriptionOptionsInit_Promise_PushSubscription
  :: PushManager
     -> (Nullable PushSubscriptionOptionsInitClass
         -> (IO (Promise PushSubscriptionClass)))
foreign import javascript safe "$1.getSubscription()" js_fun_getSubscription__Promise_nullable_PushSubscription
  :: PushManager
     -> (IO (Promise (NullableClass PushSubscriptionClass)))
foreign import javascript safe "$1.permissionState($2)" js_fun_permissionState_nullable_PushSubscriptionOptionsInit_Promise_PushPermissionState
  :: PushManager
     -> (Nullable PushSubscriptionOptionsInitClass
         -> (IO (Promise PushPermissionStateClass)))
