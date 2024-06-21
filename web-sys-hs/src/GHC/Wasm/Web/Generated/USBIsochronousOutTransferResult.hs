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
module GHC.Wasm.Web.Generated.USBIsochronousOutTransferResult (
        USBIsochronousOutTransferResult,
        USBIsochronousOutTransferResultClass,
        js_cons_USBIsochronousOutTransferResult, js_get_packets
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.USBIsochronousOutTransferPacket.Core
import GHC.Wasm.Web.Generated.USBIsochronousOutTransferResult.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "USBIsochronousOutTransferResult($1)" js_cons_USBIsochronousOutTransferResult
  :: Sequence USBIsochronousOutTransferPacketClass
     -> (IO USBIsochronousOutTransferResult)
foreign import javascript unsafe "$1.packets" js_get_packets
  :: USBIsochronousOutTransferResult
     -> (IO (FrozenArray USBIsochronousOutTransferPacketClass))
