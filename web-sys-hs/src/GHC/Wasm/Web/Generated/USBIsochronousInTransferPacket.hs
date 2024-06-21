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
module GHC.Wasm.Web.Generated.USBIsochronousInTransferPacket (
        USBIsochronousInTransferPacket,
        USBIsochronousInTransferPacketClass,
        js_cons_USBIsochronousInTransferPacket, js_get_data, js_get_status
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.USBIsochronousInTransferPacket.Core
import GHC.Wasm.Web.Generated.USBTransferStatus.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "USBIsochronousInTransferPacket($1,$2)" js_cons_USBIsochronousInTransferPacket
  :: USBTransferStatus
     -> (Nullable (NullableClass DataViewClass)
         -> (IO USBIsochronousInTransferPacket))
foreign import javascript unsafe "$1.data" js_get_data
  :: USBIsochronousInTransferPacket -> (IO (Nullable DataViewClass))
foreign import javascript unsafe "$1.status" js_get_status
  :: USBIsochronousInTransferPacket -> (IO USBTransferStatus)
