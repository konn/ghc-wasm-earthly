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
module GHC.Wasm.Web.Generated.USBDevice (
        USBDevice, USBDeviceClass, js_fun_open__Promise_undefined,
        js_fun_forget__Promise_undefined, js_fun_close__Promise_undefined,
        js_fun_selectConfiguration_octet_Promise_undefined,
        js_fun_claimInterface_octet_Promise_undefined,
        js_fun_releaseInterface_octet_Promise_undefined,
        js_fun_selectAlternateInterface_octet_octet_Promise_undefined,
        js_fun_controlTransferIn_USBControlTransferParameters_short_Promise_USBInTransferResult,
        js_fun_controlTransferOut_USBControlTransferParameters_nullable_BufferSource_Promise_USBOutTransferResult,
        js_fun_clearHalt_USBDirection_octet_Promise_undefined,
        js_fun_transferIn_octet_long_Promise_USBInTransferResult,
        js_fun_transferOut_octet_BufferSource_Promise_USBOutTransferResult,
        js_fun_isochronousTransferIn_octet_sequence_long_Promise_USBIsochronousInTransferResult,
        js_fun_isochronousTransferOut_octet_BufferSource_sequence_long_Promise_USBIsochronousOutTransferResult,
        js_fun_reset__Promise_undefined, js_get_usbVersionMajor,
        js_get_usbVersionMinor, js_get_usbVersionSubminor,
        js_get_deviceClass, js_get_deviceSubclass, js_get_deviceProtocol,
        js_get_vendorId, js_get_productId, js_get_deviceVersionMajor,
        js_get_deviceVersionMinor, js_get_deviceVersionSubminor,
        js_get_manufacturerName, js_get_productName, js_get_serialNumber,
        js_get_configuration, js_get_configurations, js_get_opened
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.USBConfiguration.Core
import GHC.Wasm.Web.Generated.USBControlTransferParameters.Core
import GHC.Wasm.Web.Generated.USBDevice.Core
import GHC.Wasm.Web.Generated.USBDirection.Core
import GHC.Wasm.Web.Generated.USBInTransferResult.Core
import GHC.Wasm.Web.Generated.USBIsochronousInTransferResult.Core
import GHC.Wasm.Web.Generated.USBIsochronousOutTransferResult.Core
import GHC.Wasm.Web.Generated.USBOutTransferResult.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.open()" js_fun_open__Promise_undefined
  :: USBDevice -> (IO (Promise UndefinedClass))
foreign import javascript safe "$1.forget()" js_fun_forget__Promise_undefined
  :: USBDevice -> (IO (Promise UndefinedClass))
foreign import javascript safe "$1.close()" js_fun_close__Promise_undefined
  :: USBDevice -> (IO (Promise UndefinedClass))
foreign import javascript safe "$1.selectConfiguration($2)" js_fun_selectConfiguration_octet_Promise_undefined
  :: USBDevice -> (Word8 -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.claimInterface($2)" js_fun_claimInterface_octet_Promise_undefined
  :: USBDevice -> (Word8 -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.releaseInterface($2)" js_fun_releaseInterface_octet_Promise_undefined
  :: USBDevice -> (Word8 -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.selectAlternateInterface($2,$3)" js_fun_selectAlternateInterface_octet_octet_Promise_undefined
  :: USBDevice -> (Word8 -> (Word8 -> (IO (Promise UndefinedClass))))
foreign import javascript safe "$1.controlTransferIn($2,$3)" js_fun_controlTransferIn_USBControlTransferParameters_short_Promise_USBInTransferResult
  :: USBDevice
     -> (USBControlTransferParameters
         -> (Word16 -> (IO (Promise USBInTransferResultClass))))
foreign import javascript safe "$1.controlTransferOut($2,$3)" js_fun_controlTransferOut_USBControlTransferParameters_nullable_BufferSource_Promise_USBOutTransferResult
  :: USBDevice
     -> (USBControlTransferParameters
         -> (Nullable BufferSourceClass
             -> (IO (Promise USBOutTransferResultClass))))
foreign import javascript safe "$1.clearHalt($2,$3)" js_fun_clearHalt_USBDirection_octet_Promise_undefined
  :: USBDevice
     -> (USBDirection -> (Word8 -> (IO (Promise UndefinedClass))))
foreign import javascript safe "$1.transferIn($2,$3)" js_fun_transferIn_octet_long_Promise_USBInTransferResult
  :: USBDevice
     -> (Word8 -> (Word32 -> (IO (Promise USBInTransferResultClass))))
foreign import javascript safe "$1.transferOut($2,$3)" js_fun_transferOut_octet_BufferSource_Promise_USBOutTransferResult
  :: USBDevice
     -> (Word8
         -> (BufferSource -> (IO (Promise USBOutTransferResultClass))))
foreign import javascript safe "$1.isochronousTransferIn($2,$3)" js_fun_isochronousTransferIn_octet_sequence_long_Promise_USBIsochronousInTransferResult
  :: USBDevice
     -> (Word8
         -> (Sequence (JSPrimClass Word32)
             -> (IO (Promise USBIsochronousInTransferResultClass))))
foreign import javascript safe "$1.isochronousTransferOut($2,$3,$4)" js_fun_isochronousTransferOut_octet_BufferSource_sequence_long_Promise_USBIsochronousOutTransferResult
  :: USBDevice
     -> (Word8
         -> (BufferSource
             -> (Sequence (JSPrimClass Word32)
                 -> (IO (Promise USBIsochronousOutTransferResultClass)))))
foreign import javascript safe "$1.reset()" js_fun_reset__Promise_undefined
  :: USBDevice -> (IO (Promise UndefinedClass))
foreign import javascript unsafe "$1.usbVersionMajor" js_get_usbVersionMajor
  :: USBDevice -> (IO Word8)
foreign import javascript unsafe "$1.usbVersionMinor" js_get_usbVersionMinor
  :: USBDevice -> (IO Word8)
foreign import javascript unsafe "$1.usbVersionSubminor" js_get_usbVersionSubminor
  :: USBDevice -> (IO Word8)
foreign import javascript unsafe "$1.deviceClass" js_get_deviceClass
  :: USBDevice -> (IO Word8)
foreign import javascript unsafe "$1.deviceSubclass" js_get_deviceSubclass
  :: USBDevice -> (IO Word8)
foreign import javascript unsafe "$1.deviceProtocol" js_get_deviceProtocol
  :: USBDevice -> (IO Word8)
foreign import javascript unsafe "$1.vendorId" js_get_vendorId
  :: USBDevice -> (IO Word16)
foreign import javascript unsafe "$1.productId" js_get_productId
  :: USBDevice -> (IO Word16)
foreign import javascript unsafe "$1.deviceVersionMajor" js_get_deviceVersionMajor
  :: USBDevice -> (IO Word8)
foreign import javascript unsafe "$1.deviceVersionMinor" js_get_deviceVersionMinor
  :: USBDevice -> (IO Word8)
foreign import javascript unsafe "$1.deviceVersionSubminor" js_get_deviceVersionSubminor
  :: USBDevice -> (IO Word8)
foreign import javascript unsafe "$1.manufacturerName" js_get_manufacturerName
  :: USBDevice -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.productName" js_get_productName
  :: USBDevice -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.serialNumber" js_get_serialNumber
  :: USBDevice -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.configuration" js_get_configuration
  :: USBDevice -> (IO (Nullable USBConfigurationClass))
foreign import javascript unsafe "$1.configurations" js_get_configurations
  :: USBDevice -> (IO (FrozenArray USBConfigurationClass))
foreign import javascript unsafe "$1.opened" js_get_opened
  :: USBDevice -> (IO Bool)
