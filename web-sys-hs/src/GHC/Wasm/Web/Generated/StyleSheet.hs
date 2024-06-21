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
module GHC.Wasm.Web.Generated.StyleSheet (
        StyleSheet, StyleSheetClass, js_get_type, js_get_href,
        js_get_ownerNode, js_get_parentStyleSheet, js_get_title,
        js_get_media, js_get_disabled, js_set_disabled,
        js_get_sourceMapURL, js_get_sourceURL
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MediaList.Core
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Generated.StyleSheet.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.type" js_get_type
  :: StyleSheet -> (IO DOMString)
foreign import javascript unsafe "$1.href" js_get_href
  :: StyleSheet -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.ownerNode" js_get_ownerNode
  :: StyleSheet -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.parentStyleSheet" js_get_parentStyleSheet
  :: StyleSheet -> (IO (Nullable StyleSheetClass))
foreign import javascript unsafe "$1.title" js_get_title
  :: StyleSheet -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.media" js_get_media
  :: StyleSheet -> (IO MediaList)
foreign import javascript unsafe "$1.disabled" js_get_disabled
  :: StyleSheet -> (IO Bool)
foreign import javascript unsafe "$1.disabled = $2" js_set_disabled
  :: StyleSheet -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.sourceMapURL" js_get_sourceMapURL
  :: StyleSheet -> (IO DOMString)
foreign import javascript unsafe "$1.sourceURL" js_get_sourceURL
  :: StyleSheet -> (IO DOMString)
