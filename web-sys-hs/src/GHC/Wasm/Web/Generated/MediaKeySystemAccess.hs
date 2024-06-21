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
module GHC.Wasm.Web.Generated.MediaKeySystemAccess (
        MediaKeySystemAccess, MediaKeySystemAccessClass,
        js_fun_getConfiguration__MediaKeySystemConfiguration,
        js_fun_createMediaKeys__Promise_MediaKeys, js_get_keySystem
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MediaKeySystemAccess.Core
import GHC.Wasm.Web.Generated.MediaKeySystemConfiguration.Core
import GHC.Wasm.Web.Generated.MediaKeys.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getConfiguration()" js_fun_getConfiguration__MediaKeySystemConfiguration
  :: MediaKeySystemAccess -> (IO MediaKeySystemConfiguration)
foreign import javascript safe "$1.createMediaKeys()" js_fun_createMediaKeys__Promise_MediaKeys
  :: MediaKeySystemAccess -> (IO (Promise MediaKeysClass))
foreign import javascript unsafe "$1.keySystem" js_get_keySystem
  :: MediaKeySystemAccess -> (IO DOMString)
