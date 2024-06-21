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
module GHC.Wasm.Web.Generated.FileSystemHandle (
        FileSystemHandle, FileSystemHandleClass,
        js_fun_isSameEntry_FileSystemHandle_Promise_boolean,
        js_fun_queryPermission_nullable_FileSystemHandlePermissionDescriptor_Promise_PermissionState,
        js_fun_requestPermission_nullable_FileSystemHandlePermissionDescriptor_Promise_PermissionState,
        js_get_kind, js_get_name
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.FileSystemHandle.Core
import GHC.Wasm.Web.Generated.FileSystemHandleKind.Core
import GHC.Wasm.Web.Generated.FileSystemHandlePermissionDescriptor.Core
import GHC.Wasm.Web.Generated.PermissionState.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.isSameEntry($2)" js_fun_isSameEntry_FileSystemHandle_Promise_boolean
  :: FileSystemHandle
     -> (FileSystemHandle -> (IO (Promise (JSPrimClass Bool))))
foreign import javascript safe "$1.queryPermission($2)" js_fun_queryPermission_nullable_FileSystemHandlePermissionDescriptor_Promise_PermissionState
  :: FileSystemHandle
     -> (Nullable FileSystemHandlePermissionDescriptorClass
         -> (IO (Promise PermissionStateClass)))
foreign import javascript safe "$1.requestPermission($2)" js_fun_requestPermission_nullable_FileSystemHandlePermissionDescriptor_Promise_PermissionState
  :: FileSystemHandle
     -> (Nullable FileSystemHandlePermissionDescriptorClass
         -> (IO (Promise PermissionStateClass)))
foreign import javascript unsafe "$1.kind" js_get_kind
  :: FileSystemHandle -> (IO FileSystemHandleKind)
foreign import javascript unsafe "$1.name" js_get_name
  :: FileSystemHandle -> (IO USVString)
