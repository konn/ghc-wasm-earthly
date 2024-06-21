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
module GHC.Wasm.Web.Generated.USBIsochronousInTransferResult (
        USBIsochronousInTransferResult,
        USBIsochronousInTransferResultClass,
        js_cons_USBIsochronousInTransferResult, js_get_data, js_get_packets
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.USBIsochronousInTransferPacket.Core
import GHC.Wasm.Web.Generated.USBIsochronousInTransferResult.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "USBIsochronousInTransferResult($1,$2)" js_cons_USBIsochronousInTransferResult
  :: Sequence USBIsochronousInTransferPacketClass
     -> (Nullable (NullableClass DataViewClass)
         -> (IO USBIsochronousInTransferResult))
foreign import javascript unsafe "$1.data" js_get_data
  :: USBIsochronousInTransferResult -> (IO (Nullable DataViewClass))
foreign import javascript unsafe "$1.packets" js_get_packets
  :: USBIsochronousInTransferResult
     -> (IO (FrozenArray USBIsochronousInTransferPacketClass))
