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
module GHC.Wasm.Web.Generated.FileSystemSyncAccessHandle (
        FileSystemSyncAccessHandle, FileSystemSyncAccessHandleClass,
        js_fun_read_BufferSource_nullable_FileSystemReadWriteOptions_longlong,
        js_fun_write_BufferSource_nullable_FileSystemReadWriteOptions_longlong,
        js_fun_truncate_longlong_undefined, js_fun_getSize__longlong,
        js_fun_flush__undefined, js_fun_close__undefined
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.FileSystemReadWriteOptions.Core
import GHC.Wasm.Web.Generated.FileSystemSyncAccessHandle.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.read($2,$3)" js_fun_read_BufferSource_nullable_FileSystemReadWriteOptions_longlong
  :: FileSystemSyncAccessHandle
     -> (BufferSource
         -> (Nullable FileSystemReadWriteOptionsClass -> (IO Word64)))
foreign import javascript unsafe "$1.write($2,$3)" js_fun_write_BufferSource_nullable_FileSystemReadWriteOptions_longlong
  :: FileSystemSyncAccessHandle
     -> (BufferSource
         -> (Nullable FileSystemReadWriteOptionsClass -> (IO Word64)))
foreign import javascript unsafe "$1.truncate($2)" js_fun_truncate_longlong_undefined
  :: FileSystemSyncAccessHandle -> (Word64 -> (IO ()))
foreign import javascript unsafe "$1.getSize()" js_fun_getSize__longlong
  :: FileSystemSyncAccessHandle -> (IO Word64)
foreign import javascript unsafe "$1.flush()" js_fun_flush__undefined
  :: FileSystemSyncAccessHandle -> (IO ())
foreign import javascript unsafe "$1.close()" js_fun_close__undefined
  :: FileSystemSyncAccessHandle -> (IO ())
