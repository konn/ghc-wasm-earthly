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
module GHC.Wasm.Web.Generated.NamedNodeMap (
        NamedNodeMap, NamedNodeMapClass,
        js_fun_setNamedItem_Attr_nullable_Attr,
        js_fun_removeNamedItem_DOMString_Attr,
        js_fun_getNamedItemNS_nullable_DOMString_DOMString_nullable_Attr,
        js_fun_setNamedItemNS_Attr_nullable_Attr,
        js_fun_removeNamedItemNS_nullable_DOMString_DOMString_Attr,
        js_get_length
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Attr.Core
import GHC.Wasm.Web.Generated.NamedNodeMap.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.setNamedItem($2)" js_fun_setNamedItem_Attr_nullable_Attr
  :: NamedNodeMap -> (Attr -> (IO (Nullable AttrClass)))
foreign import javascript unsafe "$1.removeNamedItem($2)" js_fun_removeNamedItem_DOMString_Attr
  :: NamedNodeMap -> (DOMString -> (IO Attr))
foreign import javascript unsafe "$1.getNamedItemNS($2,$3)" js_fun_getNamedItemNS_nullable_DOMString_DOMString_nullable_Attr
  :: NamedNodeMap
     -> (Nullable DOMStringClass
         -> (DOMString -> (IO (Nullable AttrClass))))
foreign import javascript unsafe "$1.setNamedItemNS($2)" js_fun_setNamedItemNS_Attr_nullable_Attr
  :: NamedNodeMap -> (Attr -> (IO (Nullable AttrClass)))
foreign import javascript unsafe "$1.removeNamedItemNS($2,$3)" js_fun_removeNamedItemNS_nullable_DOMString_DOMString_Attr
  :: NamedNodeMap
     -> (Nullable DOMStringClass -> (DOMString -> (IO Attr)))
foreign import javascript unsafe "$1.length" js_get_length
  :: NamedNodeMap -> (IO Word32)
