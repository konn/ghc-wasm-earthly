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
module GHC.Wasm.Web.Generated.Attr (
        Attr, AttrClass, js_get_localName, js_get_value, js_set_value,
        js_get_name, js_get_namespaceURI, js_get_prefix, js_get_specified
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Attr.Core
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.localName" js_get_localName
  :: Attr -> (IO DOMString)
foreign import javascript unsafe "$1.value" js_get_value
  :: Attr -> (IO DOMString)
foreign import javascript unsafe "$1.value = $2" js_set_value
  :: Attr -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.name" js_get_name
  :: Attr -> (IO DOMString)
foreign import javascript unsafe "$1.namespaceURI" js_get_namespaceURI
  :: Attr -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.prefix" js_get_prefix
  :: Attr -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.specified" js_get_specified
  :: Attr -> (IO Bool)
