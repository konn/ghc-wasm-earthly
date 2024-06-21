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
module GHC.Wasm.Web.Generated.MemoryMeasurement.Core (
        MemoryMeasurementFields, MemoryMeasurementClass, MemoryMeasurement,
        ReifiedMemoryMeasurement
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MemoryBreakdownEntry.Core
import GHC.Wasm.Web.Types
type MemoryMeasurementFields =
    '[ '("breakdown",
         NullableClass (SequenceClass MemoryBreakdownEntryClass)),
       '("bytes", NullableClass (JSPrimClass Word64))]
type MemoryMeasurementClass =
    JSDictionaryClass MemoryMeasurementFields
type MemoryMeasurement = JSObject MemoryMeasurementClass
type ReifiedMemoryMeasurement =
    ReifiedDictionary MemoryMeasurementFields
