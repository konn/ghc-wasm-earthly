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
module GHC.Wasm.Web.Generated.StorageManager (
        StorageManager, StorageManagerClass,
        js_fun_persisted__Promise_boolean, js_fun_persist__Promise_boolean,
        js_fun_estimate__Promise_StorageEstimate,
        js_fun_getDirectory__Promise_FileSystemDirectoryHandle
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.FileSystemDirectoryHandle.Core
import GHC.Wasm.Web.Generated.StorageEstimate.Core
import GHC.Wasm.Web.Generated.StorageManager.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.persisted()" js_fun_persisted__Promise_boolean
  :: StorageManager -> (IO (Promise (JSPrimClass Bool)))
foreign import javascript safe "$1.persist()" js_fun_persist__Promise_boolean
  :: StorageManager -> (IO (Promise (JSPrimClass Bool)))
foreign import javascript safe "$1.estimate()" js_fun_estimate__Promise_StorageEstimate
  :: StorageManager -> (IO (Promise StorageEstimateClass))
foreign import javascript safe "$1.getDirectory()" js_fun_getDirectory__Promise_FileSystemDirectoryHandle
  :: StorageManager -> (IO (Promise FileSystemDirectoryHandleClass))
