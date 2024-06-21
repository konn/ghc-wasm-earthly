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
module GHC.Wasm.Web.Generated.MemoryAttribution.Core (
        MemoryAttributionFields, MemoryAttributionClass, MemoryAttribution,
        ReifiedMemoryAttribution
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MemoryAttributionContainer.Core
import GHC.Wasm.Web.Types
type MemoryAttributionFields =
    '[ '("container", NullableClass MemoryAttributionContainerClass),
       '("scope", NullableClass DOMStringClass),
       '("url", NullableClass USVStringClass)]
type MemoryAttributionClass =
    JSDictionaryClass MemoryAttributionFields
type MemoryAttribution = JSObject MemoryAttributionClass
type ReifiedMemoryAttribution =
    ReifiedDictionary MemoryAttributionFields
