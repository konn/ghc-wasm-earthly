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
module GHC.Wasm.Web.Generated.USBConfiguration (
        USBConfiguration, USBConfigurationClass, js_cons_USBConfiguration,
        js_get_configurationValue, js_get_configurationName,
        js_get_interfaces
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.USBConfiguration.Core
import GHC.Wasm.Web.Generated.USBDevice.Core
import GHC.Wasm.Web.Generated.USBInterface.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "USBConfiguration($1,$2)" js_cons_USBConfiguration
  :: USBDevice -> (Word8 -> (IO USBConfiguration))
foreign import javascript unsafe "$1.configurationValue" js_get_configurationValue
  :: USBConfiguration -> (IO Word8)
foreign import javascript unsafe "$1.configurationName" js_get_configurationName
  :: USBConfiguration -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.interfaces" js_get_interfaces
  :: USBConfiguration -> (IO (FrozenArray USBInterfaceClass))
