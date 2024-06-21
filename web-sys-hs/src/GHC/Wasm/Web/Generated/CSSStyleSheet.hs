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
module GHC.Wasm.Web.Generated.CSSStyleSheet (
        CSSStyleSheet, CSSStyleSheetClass, js_cons_CSSStyleSheet,
        js_fun_insertRule_DOMString_nullable_long_long,
        js_fun_deleteRule_long_undefined,
        js_fun_replace_USVString_Promise_CSSStyleSheet,
        js_fun_replaceSync_USVString_undefined, js_get_ownerRule,
        js_get_cssRules, js_get_parsingMode
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.CSSRule.Core
import GHC.Wasm.Web.Generated.CSSRuleList.Core
import GHC.Wasm.Web.Generated.CSSStyleSheet.Core
import GHC.Wasm.Web.Generated.CSSStyleSheetParsingMode.Core
import GHC.Wasm.Web.Generated.StyleSheet.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new CSSStyleSheet()" js_cons_CSSStyleSheet
  :: IO CSSStyleSheet
foreign import javascript unsafe "$1.insertRule($2,$3)" js_fun_insertRule_DOMString_nullable_long_long
  :: CSSStyleSheet
     -> (DOMString -> (Nullable (JSPrimClass Word32) -> (IO Word32)))
foreign import javascript unsafe "$1.deleteRule($2)" js_fun_deleteRule_long_undefined
  :: CSSStyleSheet -> (Word32 -> (IO ()))
foreign import javascript safe "$1.replace($2)" js_fun_replace_USVString_Promise_CSSStyleSheet
  :: CSSStyleSheet
     -> (USVString -> (IO (Promise CSSStyleSheetClass)))
foreign import javascript unsafe "$1.replaceSync($2)" js_fun_replaceSync_USVString_undefined
  :: CSSStyleSheet -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.ownerRule" js_get_ownerRule
  :: CSSStyleSheet -> (IO (Nullable CSSRuleClass))
foreign import javascript unsafe "$1.cssRules" js_get_cssRules
  :: CSSStyleSheet -> (IO CSSRuleList)
foreign import javascript unsafe "$1.parsingMode" js_get_parsingMode
  :: CSSStyleSheet -> (IO CSSStyleSheetParsingMode)
