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
module GHC.Wasm.Web.Generated.DocumentFragment (
        DocumentFragment, DocumentFragmentClass,
        js_fun_getElementById_DOMString_nullable_Element,
        js_fun_querySelector_DOMString_nullable_Element,
        js_fun_querySelectorAll_DOMString_NodeList,
        js_fun_prepend_Union_Node_DOMString_EndUnion_undefined,
        js_fun_append_Union_Node_DOMString_EndUnion_undefined,
        js_fun_replaceChildren_Union_Node_DOMString_EndUnion_undefined,
        js_get_children, js_get_firstElementChild, js_get_lastElementChild,
        js_get_childElementCount
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DocumentFragment.Core
import GHC.Wasm.Web.Generated.Element.Core
import GHC.Wasm.Web.Generated.HTMLCollection.Core
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Generated.NodeList.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getElementById($2)" js_fun_getElementById_DOMString_nullable_Element
  :: DocumentFragment -> (DOMString -> (IO (Nullable ElementClass)))
foreign import javascript unsafe "$1.querySelector($2)" js_fun_querySelector_DOMString_nullable_Element
  :: DocumentFragment -> (DOMString -> (IO (Nullable ElementClass)))
foreign import javascript unsafe "$1.querySelectorAll($2)" js_fun_querySelectorAll_DOMString_NodeList
  :: DocumentFragment -> (DOMString -> (IO NodeList))
foreign import javascript unsafe "$1.prepend(... $2)" js_fun_prepend_Union_Node_DOMString_EndUnion_undefined
  :: DocumentFragment
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.append(... $2)" js_fun_append_Union_Node_DOMString_EndUnion_undefined
  :: DocumentFragment
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.replaceChildren(... $2)" js_fun_replaceChildren_Union_Node_DOMString_EndUnion_undefined
  :: DocumentFragment
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.children" js_get_children
  :: DocumentFragment -> (IO HTMLCollection)
foreign import javascript unsafe "$1.firstElementChild" js_get_firstElementChild
  :: DocumentFragment -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.lastElementChild" js_get_lastElementChild
  :: DocumentFragment -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.childElementCount" js_get_childElementCount
  :: DocumentFragment -> (IO Word32)
