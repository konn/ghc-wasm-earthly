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
module GHC.Wasm.Web.Generated.ServiceWorker (
        ServiceWorker, ServiceWorkerClass,
        js_fun_postMessage_any_nullable_sequence_object_undefined,
        js_get_scriptURL, js_get_state, js_get_onstatechange,
        js_set_onstatechange, js_get_onerror, js_set_onerror
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.ServiceWorker.Core
import GHC.Wasm.Web.Generated.ServiceWorkerState.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.postMessage($2,$3)" js_fun_postMessage_any_nullable_sequence_object_undefined
  :: ServiceWorker
     -> (JSAny -> (Nullable (SequenceClass AnyClass) -> (IO ())))
foreign import javascript unsafe "$1.scriptURL" js_get_scriptURL
  :: ServiceWorker -> (IO USVString)
foreign import javascript unsafe "$1.state" js_get_state
  :: ServiceWorker -> (IO ServiceWorkerState)
foreign import javascript unsafe "$1.onstatechange" js_get_onstatechange
  :: ServiceWorker -> (IO EventHandler)
foreign import javascript unsafe "$1.onstatechange = $2" js_set_onstatechange
  :: ServiceWorker -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onerror" js_get_onerror
  :: ServiceWorker -> (IO EventHandler)
foreign import javascript unsafe "$1.onerror = $2" js_set_onerror
  :: ServiceWorker -> (EventHandler -> (IO ()))
