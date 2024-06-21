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
module GHC.Wasm.Web.Generated.USBInTransferResult (
        USBInTransferResult, USBInTransferResultClass,
        js_cons_USBInTransferResult, js_get_data, js_get_status
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.USBInTransferResult.Core
import GHC.Wasm.Web.Generated.USBTransferStatus.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "USBInTransferResult($1,$2)" js_cons_USBInTransferResult
  :: USBTransferStatus
     -> (Nullable (NullableClass DataViewClass)
         -> (IO USBInTransferResult))
foreign import javascript unsafe "$1.data" js_get_data
  :: USBInTransferResult -> (IO (Nullable DataViewClass))
foreign import javascript unsafe "$1.status" js_get_status
  :: USBInTransferResult -> (IO USBTransferStatus)
