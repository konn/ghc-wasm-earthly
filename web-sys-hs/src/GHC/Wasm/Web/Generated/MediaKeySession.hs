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
module GHC.Wasm.Web.Generated.MediaKeySession (
        MediaKeySession, MediaKeySessionClass,
        js_fun_generateRequest_DOMString_BufferSource_Promise_undefined,
        js_fun_load_DOMString_Promise_boolean,
        js_fun_update_BufferSource_Promise_undefined,
        js_fun_close__Promise_undefined, js_fun_remove__Promise_undefined,
        js_get_error, js_get_sessionId, js_get_expiration, js_get_closed,
        js_get_keyStatuses, js_get_onkeystatuseschange,
        js_set_onkeystatuseschange, js_get_onmessage, js_set_onmessage
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.MediaKeyError.Core
import GHC.Wasm.Web.Generated.MediaKeySession.Core
import GHC.Wasm.Web.Generated.MediaKeyStatusMap.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.generateRequest($2,$3)" js_fun_generateRequest_DOMString_BufferSource_Promise_undefined
  :: MediaKeySession
     -> (DOMString -> (BufferSource -> (IO (Promise UndefinedClass))))
foreign import javascript safe "$1.load($2)" js_fun_load_DOMString_Promise_boolean
  :: MediaKeySession
     -> (DOMString -> (IO (Promise (JSPrimClass Bool))))
foreign import javascript safe "$1.update($2)" js_fun_update_BufferSource_Promise_undefined
  :: MediaKeySession
     -> (BufferSource -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.close()" js_fun_close__Promise_undefined
  :: MediaKeySession -> (IO (Promise UndefinedClass))
foreign import javascript safe "$1.remove()" js_fun_remove__Promise_undefined
  :: MediaKeySession -> (IO (Promise UndefinedClass))
foreign import javascript unsafe "$1.error" js_get_error
  :: MediaKeySession -> (IO (Nullable MediaKeyErrorClass))
foreign import javascript unsafe "$1.sessionId" js_get_sessionId
  :: MediaKeySession -> (IO DOMString)
foreign import javascript unsafe "$1.expiration" js_get_expiration
  :: MediaKeySession -> (IO Double)
foreign import javascript unsafe "$1.closed" js_get_closed
  :: MediaKeySession -> (IO (Promise UndefinedClass))
foreign import javascript unsafe "$1.keyStatuses" js_get_keyStatuses
  :: MediaKeySession -> (IO MediaKeyStatusMap)
foreign import javascript unsafe "$1.onkeystatuseschange" js_get_onkeystatuseschange
  :: MediaKeySession -> (IO EventHandler)
foreign import javascript unsafe "$1.onkeystatuseschange = $2" js_set_onkeystatuseschange
  :: MediaKeySession -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmessage" js_get_onmessage
  :: MediaKeySession -> (IO EventHandler)
foreign import javascript unsafe "$1.onmessage = $2" js_set_onmessage
  :: MediaKeySession -> (EventHandler -> (IO ()))
