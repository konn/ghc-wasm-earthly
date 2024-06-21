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
module GHC.Wasm.Web.Generated.ServiceWorkerContainer (
        ServiceWorkerContainer, ServiceWorkerContainerClass,
        js_fun_register_USVString_nullable_RegistrationOptions_Promise_ServiceWorkerRegistration,
        js_fun_getRegistration_nullable_USVString_Promise_any,
        js_fun_getRegistrations__Promise_sequence_ServiceWorkerRegistration,
        js_fun_getScopeForUrl_DOMString_DOMString, js_get_controller,
        js_get_ready, js_get_oncontrollerchange, js_set_oncontrollerchange,
        js_get_onerror, js_set_onerror, js_get_onmessage, js_set_onmessage
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.RegistrationOptions.Core
import GHC.Wasm.Web.Generated.ServiceWorker.Core
import GHC.Wasm.Web.Generated.ServiceWorkerContainer.Core
import GHC.Wasm.Web.Generated.ServiceWorkerRegistration.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.register($2,$3)" js_fun_register_USVString_nullable_RegistrationOptions_Promise_ServiceWorkerRegistration
  :: ServiceWorkerContainer
     -> (USVString
         -> (Nullable RegistrationOptionsClass
             -> (IO (Promise ServiceWorkerRegistrationClass))))
foreign import javascript safe "$1.getRegistration($2)" js_fun_getRegistration_nullable_USVString_Promise_any
  :: ServiceWorkerContainer
     -> (Nullable USVStringClass -> (IO (Promise AnyClass)))
foreign import javascript safe "$1.getRegistrations()" js_fun_getRegistrations__Promise_sequence_ServiceWorkerRegistration
  :: ServiceWorkerContainer
     -> (IO (Promise (SequenceClass ServiceWorkerRegistrationClass)))
foreign import javascript unsafe "$1.getScopeForUrl($2)" js_fun_getScopeForUrl_DOMString_DOMString
  :: ServiceWorkerContainer -> (DOMString -> (IO DOMString))
foreign import javascript unsafe "$1.controller" js_get_controller
  :: ServiceWorkerContainer -> (IO (Nullable ServiceWorkerClass))
foreign import javascript unsafe "$1.ready" js_get_ready
  :: ServiceWorkerContainer
     -> (IO (Promise ServiceWorkerRegistrationClass))
foreign import javascript unsafe "$1.oncontrollerchange" js_get_oncontrollerchange
  :: ServiceWorkerContainer -> (IO EventHandler)
foreign import javascript unsafe "$1.oncontrollerchange = $2" js_set_oncontrollerchange
  :: ServiceWorkerContainer -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onerror" js_get_onerror
  :: ServiceWorkerContainer -> (IO EventHandler)
foreign import javascript unsafe "$1.onerror = $2" js_set_onerror
  :: ServiceWorkerContainer -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmessage" js_get_onmessage
  :: ServiceWorkerContainer -> (IO EventHandler)
foreign import javascript unsafe "$1.onmessage = $2" js_set_onmessage
  :: ServiceWorkerContainer -> (EventHandler -> (IO ()))
