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
module GHC.Wasm.Web.Generated.NetworkInformation (
        NetworkInformation, NetworkInformationClass, js_get_type,
        js_get_ontypechange, js_set_ontypechange
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ConnectionType.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.NetworkInformation.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.type" js_get_type
  :: NetworkInformation -> (IO ConnectionType)
foreign import javascript unsafe "$1.ontypechange" js_get_ontypechange
  :: NetworkInformation -> (IO EventHandler)
foreign import javascript unsafe "$1.ontypechange = $2" js_set_ontypechange
  :: NetworkInformation -> (EventHandler -> (IO ()))
