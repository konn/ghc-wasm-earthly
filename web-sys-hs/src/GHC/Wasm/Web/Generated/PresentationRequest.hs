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
module GHC.Wasm.Web.Generated.PresentationRequest (
        PresentationRequest, PresentationRequestClass,
        js_cons_PresentationRequest_DOMString,
        js_cons_PresentationRequest_sequence_DOMString,
        js_fun_start__Promise_PresentationConnection,
        js_fun_reconnect_DOMString_Promise_PresentationConnection,
        js_fun_getAvailability__Promise_PresentationAvailability,
        js_fun_startWithDevice_DOMString_Promise_PresentationConnection,
        js_get_onconnectionavailable, js_set_onconnectionavailable
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.PresentationAvailability.Core
import GHC.Wasm.Web.Generated.PresentationConnection.Core
import GHC.Wasm.Web.Generated.PresentationRequest.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new PresentationRequest($1)" js_cons_PresentationRequest_DOMString
  :: DOMString -> (IO PresentationRequest)
foreign import javascript unsafe "new PresentationRequest($1)" js_cons_PresentationRequest_sequence_DOMString
  :: Sequence DOMStringClass -> (IO PresentationRequest)
foreign import javascript safe "$1.start()" js_fun_start__Promise_PresentationConnection
  :: PresentationRequest
     -> (IO (Promise PresentationConnectionClass))
foreign import javascript safe "$1.reconnect($2)" js_fun_reconnect_DOMString_Promise_PresentationConnection
  :: PresentationRequest
     -> (DOMString -> (IO (Promise PresentationConnectionClass)))
foreign import javascript safe "$1.getAvailability()" js_fun_getAvailability__Promise_PresentationAvailability
  :: PresentationRequest
     -> (IO (Promise PresentationAvailabilityClass))
foreign import javascript safe "$1.startWithDevice($2)" js_fun_startWithDevice_DOMString_Promise_PresentationConnection
  :: PresentationRequest
     -> (DOMString -> (IO (Promise PresentationConnectionClass)))
foreign import javascript unsafe "$1.onconnectionavailable" js_get_onconnectionavailable
  :: PresentationRequest -> (IO EventHandler)
foreign import javascript unsafe "$1.onconnectionavailable = $2" js_set_onconnectionavailable
  :: PresentationRequest -> (EventHandler -> (IO ()))
