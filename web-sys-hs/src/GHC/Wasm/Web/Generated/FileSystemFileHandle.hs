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
module GHC.Wasm.Web.Generated.FileSystemFileHandle (
        FileSystemFileHandle, FileSystemFileHandleClass,
        js_fun_getFile__Promise_File,
        js_fun_createWritable_nullable_FileSystemCreateWritableOptions_Promise_FileSystemWritableFileStream,
        js_fun_createSyncAccessHandle__Promise_FileSystemSyncAccessHandle
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.File.Core
import GHC.Wasm.Web.Generated.FileSystemCreateWritableOptions.Core
import GHC.Wasm.Web.Generated.FileSystemFileHandle.Core
import GHC.Wasm.Web.Generated.FileSystemHandle.Core
import GHC.Wasm.Web.Generated.FileSystemSyncAccessHandle.Core
import GHC.Wasm.Web.Generated.FileSystemWritableFileStream.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.getFile()" js_fun_getFile__Promise_File
  :: FileSystemFileHandle -> (IO (Promise FileClass))
foreign import javascript safe "$1.createWritable($2)" js_fun_createWritable_nullable_FileSystemCreateWritableOptions_Promise_FileSystemWritableFileStream
  :: FileSystemFileHandle
     -> (Nullable FileSystemCreateWritableOptionsClass
         -> (IO (Promise FileSystemWritableFileStreamClass)))
foreign import javascript safe "$1.createSyncAccessHandle()" js_fun_createSyncAccessHandle__Promise_FileSystemSyncAccessHandle
  :: FileSystemFileHandle
     -> (IO (Promise FileSystemSyncAccessHandleClass))
