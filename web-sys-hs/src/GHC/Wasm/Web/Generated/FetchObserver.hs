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
module GHC.Wasm.Web.Generated.FetchObserver (
        FetchObserver, FetchObserverClass, js_get_state,
        js_get_onstatechange, js_set_onstatechange,
        js_get_onrequestprogress, js_set_onrequestprogress,
        js_get_onresponseprogress, js_set_onresponseprogress
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.FetchObserver.Core
import GHC.Wasm.Web.Generated.FetchState.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.state" js_get_state
  :: FetchObserver -> (IO FetchState)
foreign import javascript unsafe "$1.onstatechange" js_get_onstatechange
  :: FetchObserver -> (IO EventHandler)
foreign import javascript unsafe "$1.onstatechange = $2" js_set_onstatechange
  :: FetchObserver -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onrequestprogress" js_get_onrequestprogress
  :: FetchObserver -> (IO EventHandler)
foreign import javascript unsafe "$1.onrequestprogress = $2" js_set_onrequestprogress
  :: FetchObserver -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onresponseprogress" js_get_onresponseprogress
  :: FetchObserver -> (IO EventHandler)
foreign import javascript unsafe "$1.onresponseprogress = $2" js_set_onresponseprogress
  :: FetchObserver -> (EventHandler -> (IO ()))
