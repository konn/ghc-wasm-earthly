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
module GHC.Wasm.Web.Generated.DirectoryPickerOptions.Core (
        DirectoryPickerOptionsFields, DirectoryPickerOptionsClass,
        DirectoryPickerOptions, ReifiedDirectoryPickerOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.FileSystemPermissionMode.Core
import GHC.Wasm.Web.Generated.StartInDirectory.Core
import GHC.Wasm.Web.Types
type DirectoryPickerOptionsFields =
    '[ '("id", NullableClass DOMStringClass),
       '("mode", NullableClass FileSystemPermissionModeClass),
       '("startIn", NullableClass StartInDirectoryClass)]
type DirectoryPickerOptionsClass =
    JSDictionaryClass DirectoryPickerOptionsFields
type DirectoryPickerOptions = JSObject DirectoryPickerOptionsClass
type ReifiedDirectoryPickerOptions =
    ReifiedDictionary DirectoryPickerOptionsFields
