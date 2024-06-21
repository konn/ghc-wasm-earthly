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
module GHC.Wasm.Web.Generated.MemoryAttributionContainer.Core (
        MemoryAttributionContainerFields, MemoryAttributionContainerClass,
        MemoryAttributionContainer, ReifiedMemoryAttributionContainer
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type MemoryAttributionContainerFields =
    '[ '("id", NullableClass DOMStringClass),
       '("src", NullableClass USVStringClass)]
type MemoryAttributionContainerClass =
    JSDictionaryClass MemoryAttributionContainerFields
type MemoryAttributionContainer =
    JSObject MemoryAttributionContainerClass
type ReifiedMemoryAttributionContainer =
    ReifiedDictionary MemoryAttributionContainerFields
