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
module GHC.Wasm.Web.Generated.ShadowRoot (
        ShadowRoot, ShadowRootClass,
        js_fun_getElementById_DOMString_nullable_Element,
        js_fun_getElementsByTagName_DOMString_HTMLCollection,
        js_fun_getElementsByTagNameNS_nullable_DOMString_DOMString_HTMLCollection,
        js_fun_getElementsByClassName_DOMString_HTMLCollection,
        js_fun_elementFromPoint_float_float_nullable_Element,
        js_fun_elementsFromPoint_float_float_sequence_Element, js_get_mode,
        js_get_host, js_get_innerHTML, js_set_innerHTML,
        js_get_activeElement, js_get_styleSheets,
        js_get_pointerLockElement, js_get_fullscreenElement
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DocumentFragment.Core
import GHC.Wasm.Web.Generated.Element.Core
import GHC.Wasm.Web.Generated.HTMLCollection.Core
import GHC.Wasm.Web.Generated.ShadowRoot.Core
import GHC.Wasm.Web.Generated.ShadowRootMode.Core
import GHC.Wasm.Web.Generated.StyleSheetList.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getElementById($2)" js_fun_getElementById_DOMString_nullable_Element
  :: ShadowRoot -> (DOMString -> (IO (Nullable ElementClass)))
foreign import javascript unsafe "$1.getElementsByTagName($2)" js_fun_getElementsByTagName_DOMString_HTMLCollection
  :: ShadowRoot -> (DOMString -> (IO HTMLCollection))
foreign import javascript unsafe "$1.getElementsByTagNameNS($2,$3)" js_fun_getElementsByTagNameNS_nullable_DOMString_DOMString_HTMLCollection
  :: ShadowRoot
     -> (Nullable DOMStringClass -> (DOMString -> (IO HTMLCollection)))
foreign import javascript unsafe "$1.getElementsByClassName($2)" js_fun_getElementsByClassName_DOMString_HTMLCollection
  :: ShadowRoot -> (DOMString -> (IO HTMLCollection))
foreign import javascript unsafe "$1.elementFromPoint($2,$3)" js_fun_elementFromPoint_float_float_nullable_Element
  :: ShadowRoot -> (Float -> (Float -> (IO (Nullable ElementClass))))
foreign import javascript unsafe "$1.elementsFromPoint($2,$3)" js_fun_elementsFromPoint_float_float_sequence_Element
  :: ShadowRoot -> (Float -> (Float -> (IO (Sequence ElementClass))))
foreign import javascript unsafe "$1.mode" js_get_mode
  :: ShadowRoot -> (IO ShadowRootMode)
foreign import javascript unsafe "$1.host" js_get_host
  :: ShadowRoot -> (IO Element)
foreign import javascript unsafe "$1.innerHTML" js_get_innerHTML
  :: ShadowRoot -> (IO DOMString)
foreign import javascript unsafe "$1.innerHTML = $2" js_set_innerHTML
  :: ShadowRoot -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.activeElement" js_get_activeElement
  :: ShadowRoot -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.styleSheets" js_get_styleSheets
  :: ShadowRoot -> (IO StyleSheetList)
foreign import javascript unsafe "$1.pointerLockElement" js_get_pointerLockElement
  :: ShadowRoot -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.fullscreenElement" js_get_fullscreenElement
  :: ShadowRoot -> (IO (Nullable ElementClass))
