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
module GHC.Wasm.Web.Generated.PluginArray (
        PluginArray, PluginArrayClass,
        js_fun_refresh_nullable_boolean_undefined, js_get_length
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Plugin.Core
import GHC.Wasm.Web.Generated.PluginArray.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.refresh($2)" js_fun_refresh_nullable_boolean_undefined
  :: PluginArray -> (Nullable (JSPrimClass Bool) -> (IO ()))
foreign import javascript unsafe "$1.length" js_get_length
  :: PluginArray -> (IO Word32)
