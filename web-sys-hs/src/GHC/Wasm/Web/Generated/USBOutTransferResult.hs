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
module GHC.Wasm.Web.Generated.USBOutTransferResult (
        USBOutTransferResult, USBOutTransferResultClass,
        js_cons_USBOutTransferResult, js_get_bytesWritten, js_get_status
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.USBOutTransferResult.Core
import GHC.Wasm.Web.Generated.USBTransferStatus.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "USBOutTransferResult($1,$2)" js_cons_USBOutTransferResult
  :: USBTransferStatus
     -> (Nullable (JSPrimClass Word32) -> (IO USBOutTransferResult))
foreign import javascript unsafe "$1.bytesWritten" js_get_bytesWritten
  :: USBOutTransferResult -> (IO Word32)
foreign import javascript unsafe "$1.status" js_get_status
  :: USBOutTransferResult -> (IO USBTransferStatus)
