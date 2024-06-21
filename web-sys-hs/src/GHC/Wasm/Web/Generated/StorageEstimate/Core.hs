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
module GHC.Wasm.Web.Generated.StorageEstimate.Core (
        StorageEstimateFields, StorageEstimateClass, StorageEstimate,
        ReifiedStorageEstimate
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type StorageEstimateFields =
    '[ '("quota", NullableClass (JSPrimClass Word64)),
       '("usage", NullableClass (JSPrimClass Word64))]
type StorageEstimateClass = JSDictionaryClass StorageEstimateFields
type StorageEstimate = JSObject StorageEstimateClass
type ReifiedStorageEstimate =
    ReifiedDictionary StorageEstimateFields
