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
module GHC.Wasm.Web.Generated.Plugin (
        Plugin, PluginClass, js_get_description, js_get_filename,
        js_get_version, js_get_name, js_get_length
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MimeType.Core
import GHC.Wasm.Web.Generated.Plugin.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.description" js_get_description
  :: Plugin -> (IO DOMString)
foreign import javascript unsafe "$1.filename" js_get_filename
  :: Plugin -> (IO DOMString)
foreign import javascript unsafe "$1.version" js_get_version
  :: Plugin -> (IO DOMString)
foreign import javascript unsafe "$1.name" js_get_name
  :: Plugin -> (IO DOMString)
foreign import javascript unsafe "$1.length" js_get_length
  :: Plugin -> (IO Word32)
