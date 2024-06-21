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
module GHC.Wasm.Web.Generated.NotificationPermissionCallback.Core (
        NotificationPermissionCallbackClass,
        NotificationPermissionCallback,
        js_mk_callback_NotificationPermissionCallback_impure
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.NotificationPermission.Core
import GHC.Wasm.Web.Types
type data NotificationPermissionCallbackClass :: Prototype
type instance SuperclassOf NotificationPermissionCallbackClass = 'Nothing
type NotificationPermissionCallback =
    JSObject NotificationPermissionCallbackClass
foreign import javascript unsafe "wrapper" js_mk_callback_NotificationPermissionCallback_impure
  :: (NotificationPermission -> (IO ()))
     -> NotificationPermissionCallback
