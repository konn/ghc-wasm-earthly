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
module GHC.Wasm.Web.Generated.CSSStyleDeclaration (
        CSSStyleDeclaration, CSSStyleDeclarationClass,
        js_fun_getCSSImageURLs_DOMString_sequence_DOMString,
        js_fun_getPropertyValue_DOMString_DOMString,
        js_fun_getPropertyPriority_DOMString_DOMString,
        js_fun_setProperty_DOMString_DOMString_nullable_DOMString_undefined,
        js_fun_removeProperty_DOMString_DOMString, js_get_cssText,
        js_set_cssText, js_get_length, js_get_parentRule
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.CSSRule.Core
import GHC.Wasm.Web.Generated.CSSStyleDeclaration.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getCSSImageURLs($2)" js_fun_getCSSImageURLs_DOMString_sequence_DOMString
  :: CSSStyleDeclaration
     -> (DOMString -> (IO (Sequence DOMStringClass)))
foreign import javascript unsafe "$1.getPropertyValue($2)" js_fun_getPropertyValue_DOMString_DOMString
  :: CSSStyleDeclaration -> (DOMString -> (IO DOMString))
foreign import javascript unsafe "$1.getPropertyPriority($2)" js_fun_getPropertyPriority_DOMString_DOMString
  :: CSSStyleDeclaration -> (DOMString -> (IO DOMString))
foreign import javascript unsafe "$1.setProperty($2,$3,$4)" js_fun_setProperty_DOMString_DOMString_nullable_DOMString_undefined
  :: CSSStyleDeclaration
     -> (DOMString
         -> (DOMString -> (Nullable DOMStringClass -> (IO ()))))
foreign import javascript unsafe "$1.removeProperty($2)" js_fun_removeProperty_DOMString_DOMString
  :: CSSStyleDeclaration -> (DOMString -> (IO DOMString))
foreign import javascript unsafe "$1.cssText" js_get_cssText
  :: CSSStyleDeclaration -> (IO DOMString)
foreign import javascript unsafe "$1.cssText = $2" js_set_cssText
  :: CSSStyleDeclaration -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.length" js_get_length
  :: CSSStyleDeclaration -> (IO Word32)
foreign import javascript unsafe "$1.parentRule" js_get_parentRule
  :: CSSStyleDeclaration -> (IO (Nullable CSSRuleClass))
