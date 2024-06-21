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
module GHC.Wasm.Web.Generated.MimeType (
        MimeType, MimeTypeClass, js_get_description, js_get_enabledPlugin,
        js_get_suffixes, js_get_type
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MimeType.Core
import GHC.Wasm.Web.Generated.Plugin.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.description" js_get_description
  :: MimeType -> (IO DOMString)
foreign import javascript unsafe "$1.enabledPlugin" js_get_enabledPlugin
  :: MimeType -> (IO (Nullable PluginClass))
foreign import javascript unsafe "$1.suffixes" js_get_suffixes
  :: MimeType -> (IO DOMString)
foreign import javascript unsafe "$1.type" js_get_type
  :: MimeType -> (IO DOMString)
