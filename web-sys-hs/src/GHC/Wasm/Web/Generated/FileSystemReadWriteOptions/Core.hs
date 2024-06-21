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
module GHC.Wasm.Web.Generated.FileSystemReadWriteOptions.Core (
        FileSystemReadWriteOptionsFields, FileSystemReadWriteOptionsClass,
        FileSystemReadWriteOptions, ReifiedFileSystemReadWriteOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type FileSystemReadWriteOptionsFields =
    '[ '("at", NullableClass (JSPrimClass Word64))]
type FileSystemReadWriteOptionsClass =
    JSDictionaryClass FileSystemReadWriteOptionsFields
type FileSystemReadWriteOptions =
    JSObject FileSystemReadWriteOptionsClass
type ReifiedFileSystemReadWriteOptions =
    ReifiedDictionary FileSystemReadWriteOptionsFields
