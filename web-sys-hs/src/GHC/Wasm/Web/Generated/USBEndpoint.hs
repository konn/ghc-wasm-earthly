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
module GHC.Wasm.Web.Generated.USBEndpoint (
        USBEndpoint, USBEndpointClass, js_cons_USBEndpoint,
        js_get_endpointNumber, js_get_direction, js_get_type,
        js_get_packetSize
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.USBAlternateInterface.Core
import GHC.Wasm.Web.Generated.USBDirection.Core
import GHC.Wasm.Web.Generated.USBEndpoint.Core
import GHC.Wasm.Web.Generated.USBEndpointType.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "USBEndpoint($1,$2,$3)" js_cons_USBEndpoint
  :: USBAlternateInterface
     -> (Word8 -> (USBDirection -> (IO USBEndpoint)))
foreign import javascript unsafe "$1.endpointNumber" js_get_endpointNumber
  :: USBEndpoint -> (IO Word8)
foreign import javascript unsafe "$1.direction" js_get_direction
  :: USBEndpoint -> (IO USBDirection)
foreign import javascript unsafe "$1.type" js_get_type
  :: USBEndpoint -> (IO USBEndpointType)
foreign import javascript unsafe "$1.packetSize" js_get_packetSize
  :: USBEndpoint -> (IO Word32)
