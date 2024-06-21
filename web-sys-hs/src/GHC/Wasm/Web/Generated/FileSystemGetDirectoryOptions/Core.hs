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
module GHC.Wasm.Web.Generated.FileSystemGetDirectoryOptions.Core (
        FileSystemGetDirectoryOptionsFields,
        FileSystemGetDirectoryOptionsClass, FileSystemGetDirectoryOptions,
        ReifiedFileSystemGetDirectoryOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type FileSystemGetDirectoryOptionsFields =
    '[ '("create", NullableClass (JSPrimClass Bool))]
type FileSystemGetDirectoryOptionsClass =
    JSDictionaryClass FileSystemGetDirectoryOptionsFields
type FileSystemGetDirectoryOptions =
    JSObject FileSystemGetDirectoryOptionsClass
type ReifiedFileSystemGetDirectoryOptions =
    ReifiedDictionary FileSystemGetDirectoryOptionsFields
