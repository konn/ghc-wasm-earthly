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
module GHC.Wasm.Web.Generated.FileSystemDirectoryHandle (
        FileSystemDirectoryHandle, FileSystemDirectoryHandleClass,
        js_fun_getFileHandle_USVString_nullable_FileSystemGetFileOptions_Promise_FileSystemFileHandle,
        js_fun_getDirectoryHandle_USVString_nullable_FileSystemGetDirectoryOptions_Promise_FileSystemDirectoryHandle,
        js_fun_removeEntry_USVString_nullable_FileSystemRemoveOptions_Promise_undefined,
        js_fun_resolve_FileSystemHandle_Promise_nullable_sequence_USVString,
        js_asynciter_FileSystemDirectoryHandle_USVString_FileSystemHandle
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.FileSystemDirectoryHandle.Core
import GHC.Wasm.Web.Generated.FileSystemFileHandle.Core
import GHC.Wasm.Web.Generated.FileSystemGetDirectoryOptions.Core
import GHC.Wasm.Web.Generated.FileSystemGetFileOptions.Core
import GHC.Wasm.Web.Generated.FileSystemHandle.Core
import GHC.Wasm.Web.Generated.FileSystemRemoveOptions.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.getFileHandle($2,$3)" js_fun_getFileHandle_USVString_nullable_FileSystemGetFileOptions_Promise_FileSystemFileHandle
  :: FileSystemDirectoryHandle
     -> (USVString
         -> (Nullable FileSystemGetFileOptionsClass
             -> (IO (Promise FileSystemFileHandleClass))))
foreign import javascript safe "$1.getDirectoryHandle($2,$3)" js_fun_getDirectoryHandle_USVString_nullable_FileSystemGetDirectoryOptions_Promise_FileSystemDirectoryHandle
  :: FileSystemDirectoryHandle
     -> (USVString
         -> (Nullable FileSystemGetDirectoryOptionsClass
             -> (IO (Promise FileSystemDirectoryHandleClass))))
foreign import javascript safe "$1.removeEntry($2,$3)" js_fun_removeEntry_USVString_nullable_FileSystemRemoveOptions_Promise_undefined
  :: FileSystemDirectoryHandle
     -> (USVString
         -> (Nullable FileSystemRemoveOptionsClass
             -> (IO (Promise UndefinedClass))))
foreign import javascript safe "$1.resolve($2)" js_fun_resolve_FileSystemHandle_Promise_nullable_sequence_USVString
  :: FileSystemDirectoryHandle
     -> (FileSystemHandle
         -> (IO (Promise (NullableClass (SequenceClass USVStringClass)))))
foreign import javascript unsafe "$1[Symbol.asyncIterator]()" js_asynciter_FileSystemDirectoryHandle_USVString_FileSystemHandle
  :: FileSystemDirectoryHandle
     -> (IO (PairAsyncIterable USVStringClass FileSystemHandleClass))
