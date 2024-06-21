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
module GHC.Wasm.Web.Generated.MemoryBreakdownEntry.Core (
        MemoryBreakdownEntryFields, MemoryBreakdownEntryClass,
        MemoryBreakdownEntry, ReifiedMemoryBreakdownEntry
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MemoryAttribution.Core
import GHC.Wasm.Web.Types
type MemoryBreakdownEntryFields =
    '[ '("attribution",
         NullableClass (SequenceClass MemoryAttributionClass)),
       '("bytes", NullableClass (JSPrimClass Word64)),
       '("types", NullableClass (SequenceClass DOMStringClass))]
type MemoryBreakdownEntryClass =
    JSDictionaryClass MemoryBreakdownEntryFields
type MemoryBreakdownEntry = JSObject MemoryBreakdownEntryClass
type ReifiedMemoryBreakdownEntry =
    ReifiedDictionary MemoryBreakdownEntryFields
