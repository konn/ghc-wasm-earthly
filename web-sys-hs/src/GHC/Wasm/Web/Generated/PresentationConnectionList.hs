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
module GHC.Wasm.Web.Generated.PresentationConnectionList (
        PresentationConnectionList, PresentationConnectionListClass,
        js_get_connections, js_get_onconnectionavailable,
        js_set_onconnectionavailable
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.PresentationConnection.Core
import GHC.Wasm.Web.Generated.PresentationConnectionList.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.connections" js_get_connections
  :: PresentationConnectionList
     -> (IO (Sequence PresentationConnectionClass))
foreign import javascript unsafe "$1.onconnectionavailable" js_get_onconnectionavailable
  :: PresentationConnectionList -> (IO EventHandler)
foreign import javascript unsafe "$1.onconnectionavailable = $2" js_set_onconnectionavailable
  :: PresentationConnectionList -> (EventHandler -> (IO ()))
