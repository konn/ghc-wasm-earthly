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
module GHC.Wasm.Web.Generated.Permissions (
        Permissions, PermissionsClass,
        js_fun_query_object_Promise_PermissionStatus,
        js_fun_revoke_object_Promise_PermissionStatus
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.PermissionStatus.Core
import GHC.Wasm.Web.Generated.Permissions.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.query($2)" js_fun_query_object_Promise_PermissionStatus
  :: Permissions -> (JSAny -> (IO (Promise PermissionStatusClass)))
foreign import javascript safe "$1.revoke($2)" js_fun_revoke_object_Promise_PermissionStatus
  :: Permissions -> (JSAny -> (IO (Promise PermissionStatusClass)))
