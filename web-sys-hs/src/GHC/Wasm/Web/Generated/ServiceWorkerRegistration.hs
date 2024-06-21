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
module GHC.Wasm.Web.Generated.ServiceWorkerRegistration (
        ServiceWorkerRegistration, ServiceWorkerRegistrationClass,
        js_fun_update__Promise_undefined,
        js_fun_unregister__Promise_boolean,
        js_fun_showNotification_DOMString_nullable_NotificationOptions_Promise_undefined,
        js_fun_getNotifications_nullable_GetNotificationOptions_Promise_sequence_Notification,
        js_get_installing, js_get_waiting, js_get_active, js_get_scope,
        js_get_updateViaCache, js_get_onupdatefound, js_set_onupdatefound,
        js_get_pushManager
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.GetNotificationOptions.Core
import GHC.Wasm.Web.Generated.Notification.Core
import GHC.Wasm.Web.Generated.NotificationOptions.Core
import GHC.Wasm.Web.Generated.PushManager.Core
import GHC.Wasm.Web.Generated.ServiceWorker.Core
import GHC.Wasm.Web.Generated.ServiceWorkerRegistration.Core
import GHC.Wasm.Web.Generated.ServiceWorkerUpdateViaCache.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.update()" js_fun_update__Promise_undefined
  :: ServiceWorkerRegistration -> (IO (Promise UndefinedClass))
foreign import javascript safe "$1.unregister()" js_fun_unregister__Promise_boolean
  :: ServiceWorkerRegistration -> (IO (Promise (JSPrimClass Bool)))
foreign import javascript safe "$1.showNotification($2,$3)" js_fun_showNotification_DOMString_nullable_NotificationOptions_Promise_undefined
  :: ServiceWorkerRegistration
     -> (DOMString
         -> (Nullable NotificationOptionsClass
             -> (IO (Promise UndefinedClass))))
foreign import javascript safe "$1.getNotifications($2)" js_fun_getNotifications_nullable_GetNotificationOptions_Promise_sequence_Notification
  :: ServiceWorkerRegistration
     -> (Nullable GetNotificationOptionsClass
         -> (IO (Promise (SequenceClass NotificationClass))))
foreign import javascript unsafe "$1.installing" js_get_installing
  :: ServiceWorkerRegistration -> (IO (Nullable ServiceWorkerClass))
foreign import javascript unsafe "$1.waiting" js_get_waiting
  :: ServiceWorkerRegistration -> (IO (Nullable ServiceWorkerClass))
foreign import javascript unsafe "$1.active" js_get_active
  :: ServiceWorkerRegistration -> (IO (Nullable ServiceWorkerClass))
foreign import javascript unsafe "$1.scope" js_get_scope
  :: ServiceWorkerRegistration -> (IO USVString)
foreign import javascript unsafe "$1.updateViaCache" js_get_updateViaCache
  :: ServiceWorkerRegistration -> (IO ServiceWorkerUpdateViaCache)
foreign import javascript unsafe "$1.onupdatefound" js_get_onupdatefound
  :: ServiceWorkerRegistration -> (IO EventHandler)
foreign import javascript unsafe "$1.onupdatefound = $2" js_set_onupdatefound
  :: ServiceWorkerRegistration -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.pushManager" js_get_pushManager
  :: ServiceWorkerRegistration -> (IO PushManager)
