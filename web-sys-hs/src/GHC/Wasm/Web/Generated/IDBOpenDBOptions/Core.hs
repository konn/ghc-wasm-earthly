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
module GHC.Wasm.Web.Generated.IDBOpenDBOptions.Core (
        IDBOpenDBOptionsFields, IDBOpenDBOptionsClass, IDBOpenDBOptions,
        ReifiedIDBOpenDBOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.StorageType.Core
import GHC.Wasm.Web.Types
type IDBOpenDBOptionsFields =
    '[ '("storage", NullableClass StorageTypeClass),
       '("version", NullableClass (JSPrimClass Word64))]
type IDBOpenDBOptionsClass =
    JSDictionaryClass IDBOpenDBOptionsFields
type IDBOpenDBOptions = JSObject IDBOpenDBOptionsClass
type ReifiedIDBOpenDBOptions =
    ReifiedDictionary IDBOpenDBOptionsFields
