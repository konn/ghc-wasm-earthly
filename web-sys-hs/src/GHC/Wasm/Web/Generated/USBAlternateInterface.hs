{-# OPTIONS_GHC -Wno-unused-imports #-}
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
module GHC.Wasm.Web.Generated.USBAlternateInterface (
        USBAlternateInterface, USBAlternateInterfaceClass,
        js_cons_USBAlternateInterface, js_get_alternateSetting,
        js_get_interfaceClass, js_get_interfaceSubclass,
        js_get_interfaceProtocol, js_get_interfaceName, js_get_endpoints
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.USBAlternateInterface.Core
import GHC.Wasm.Web.Generated.USBEndpoint.Core
import GHC.Wasm.Web.Generated.USBInterface.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "USBAlternateInterface($1,$2)" js_cons_USBAlternateInterface
  :: USBInterface -> (Word8 -> (IO USBAlternateInterface))
foreign import javascript unsafe "$1.alternateSetting" js_get_alternateSetting
  :: USBAlternateInterface -> (IO Word8)
foreign import javascript unsafe "$1.interfaceClass" js_get_interfaceClass
  :: USBAlternateInterface -> (IO Word8)
foreign import javascript unsafe "$1.interfaceSubclass" js_get_interfaceSubclass
  :: USBAlternateInterface -> (IO Word8)
foreign import javascript unsafe "$1.interfaceProtocol" js_get_interfaceProtocol
  :: USBAlternateInterface -> (IO Word8)
foreign import javascript unsafe "$1.interfaceName" js_get_interfaceName
  :: USBAlternateInterface -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.endpoints" js_get_endpoints
  :: USBAlternateInterface -> (IO (FrozenArray USBEndpointClass))
