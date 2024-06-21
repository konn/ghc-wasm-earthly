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
module GHC.Wasm.Web.Generated.MediaDeviceInfo (
        MediaDeviceInfo, MediaDeviceInfoClass, js_fun_toJSON__object,
        js_get_deviceId, js_get_kind, js_get_label, js_get_groupId
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MediaDeviceInfo.Core
import GHC.Wasm.Web.Generated.MediaDeviceKind.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.toJSON()" js_fun_toJSON__object
  :: MediaDeviceInfo -> (IO JSAny)
foreign import javascript unsafe "$1.deviceId" js_get_deviceId
  :: MediaDeviceInfo -> (IO DOMString)
foreign import javascript unsafe "$1.kind" js_get_kind
  :: MediaDeviceInfo -> (IO MediaDeviceKind)
foreign import javascript unsafe "$1.label" js_get_label
  :: MediaDeviceInfo -> (IO DOMString)
foreign import javascript unsafe "$1.groupId" js_get_groupId
  :: MediaDeviceInfo -> (IO DOMString)
