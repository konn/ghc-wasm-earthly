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
module GHC.Wasm.Web.Generated.Notification (
        Notification, NotificationClass, js_cons_Notification,
        js_fun_close__undefined, js_get_onclick, js_set_onclick,
        js_get_onshow, js_set_onshow, js_get_onerror, js_set_onerror,
        js_get_onclose, js_set_onclose, js_get_title, js_get_dir,
        js_get_lang, js_get_body, js_get_tag, js_get_image, js_get_icon,
        js_get_badge, js_get_vibrate, js_get_timestamp, js_get_renotify,
        js_get_silent, js_get_requireInteraction, js_get_data,
        js_get_actions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.Notification.Core
import GHC.Wasm.Web.Generated.NotificationAction.Core
import GHC.Wasm.Web.Generated.NotificationDirection.Core
import GHC.Wasm.Web.Generated.NotificationOptions.Core
import GHC.Wasm.Web.Generated.NotificationPermission.Core
import GHC.Wasm.Web.Generated.NotificationPermissionCallback.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new Notification($1,$2)" js_cons_Notification
  :: DOMString
     -> (Nullable NotificationOptionsClass -> (IO Notification))
foreign import javascript unsafe "$1.close()" js_fun_close__undefined
  :: Notification -> (IO ())
foreign import javascript unsafe "$1.onclick" js_get_onclick
  :: Notification -> (IO EventHandler)
foreign import javascript unsafe "$1.onclick = $2" js_set_onclick
  :: Notification -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onshow" js_get_onshow
  :: Notification -> (IO EventHandler)
foreign import javascript unsafe "$1.onshow = $2" js_set_onshow
  :: Notification -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onerror" js_get_onerror
  :: Notification -> (IO EventHandler)
foreign import javascript unsafe "$1.onerror = $2" js_set_onerror
  :: Notification -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onclose" js_get_onclose
  :: Notification -> (IO EventHandler)
foreign import javascript unsafe "$1.onclose = $2" js_set_onclose
  :: Notification -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.title" js_get_title
  :: Notification -> (IO DOMString)
foreign import javascript unsafe "$1.dir" js_get_dir
  :: Notification -> (IO NotificationDirection)
foreign import javascript unsafe "$1.lang" js_get_lang
  :: Notification -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.body" js_get_body
  :: Notification -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.tag" js_get_tag
  :: Notification -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.image" js_get_image
  :: Notification -> (IO USVString)
foreign import javascript unsafe "$1.icon" js_get_icon
  :: Notification -> (IO (Nullable USVStringClass))
foreign import javascript unsafe "$1.badge" js_get_badge
  :: Notification -> (IO USVString)
foreign import javascript unsafe "$1.vibrate" js_get_vibrate
  :: Notification -> (IO (FrozenArray (JSPrimClass Word32)))
foreign import javascript unsafe "$1.timestamp" js_get_timestamp
  :: Notification -> (IO Word64)
foreign import javascript unsafe "$1.renotify" js_get_renotify
  :: Notification -> (IO Bool)
foreign import javascript unsafe "$1.silent" js_get_silent
  :: Notification -> (IO (Nullable (JSPrimClass Bool)))
foreign import javascript unsafe "$1.requireInteraction" js_get_requireInteraction
  :: Notification -> (IO Bool)
foreign import javascript unsafe "$1.data" js_get_data
  :: Notification -> (IO JSAny)
foreign import javascript unsafe "$1.actions" js_get_actions
  :: Notification -> (IO (FrozenArray NotificationActionClass))
