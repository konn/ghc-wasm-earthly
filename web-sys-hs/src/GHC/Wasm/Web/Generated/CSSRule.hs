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
module GHC.Wasm.Web.Generated.CSSRule (
        CSSRule, CSSRuleClass, js_const_CSSRule_STYLE_RULE,
        js_const_CSSRule_CHARSET_RULE, js_const_CSSRule_IMPORT_RULE,
        js_const_CSSRule_MEDIA_RULE, js_const_CSSRule_FONT_FACE_RULE,
        js_const_CSSRule_PAGE_RULE, js_const_CSSRule_NAMESPACE_RULE,
        js_const_CSSRule_KEYFRAMES_RULE, js_const_CSSRule_KEYFRAME_RULE,
        js_const_CSSRule_COUNTER_STYLE_RULE,
        js_const_CSSRule_SUPPORTS_RULE, js_const_CSSRule_DOCUMENT_RULE,
        js_const_CSSRule_FONT_FEATURE_VALUES_RULE, js_get_type,
        js_get_cssText, js_set_cssText, js_get_parentRule,
        js_get_parentStyleSheet
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.CSSRule.Core
import GHC.Wasm.Web.Generated.CSSStyleSheet.Core
import GHC.Wasm.Web.Types
js_const_CSSRule_STYLE_RULE :: Word16
js_const_CSSRule_STYLE_RULE = 1
js_const_CSSRule_CHARSET_RULE :: Word16
js_const_CSSRule_CHARSET_RULE = 2
js_const_CSSRule_IMPORT_RULE :: Word16
js_const_CSSRule_IMPORT_RULE = 3
js_const_CSSRule_MEDIA_RULE :: Word16
js_const_CSSRule_MEDIA_RULE = 4
js_const_CSSRule_FONT_FACE_RULE :: Word16
js_const_CSSRule_FONT_FACE_RULE = 5
js_const_CSSRule_PAGE_RULE :: Word16
js_const_CSSRule_PAGE_RULE = 6
js_const_CSSRule_NAMESPACE_RULE :: Word16
js_const_CSSRule_NAMESPACE_RULE = 10
js_const_CSSRule_KEYFRAMES_RULE :: Word16
js_const_CSSRule_KEYFRAMES_RULE = 7
js_const_CSSRule_KEYFRAME_RULE :: Word16
js_const_CSSRule_KEYFRAME_RULE = 8
js_const_CSSRule_COUNTER_STYLE_RULE :: Word16
js_const_CSSRule_COUNTER_STYLE_RULE = 11
js_const_CSSRule_SUPPORTS_RULE :: Word16
js_const_CSSRule_SUPPORTS_RULE = 12
js_const_CSSRule_DOCUMENT_RULE :: Word16
js_const_CSSRule_DOCUMENT_RULE = 13
js_const_CSSRule_FONT_FEATURE_VALUES_RULE :: Word16
js_const_CSSRule_FONT_FEATURE_VALUES_RULE = 14
foreign import javascript unsafe "$1.type" js_get_type
  :: CSSRule -> (IO Word16)
foreign import javascript unsafe "$1.cssText" js_get_cssText
  :: CSSRule -> (IO DOMString)
foreign import javascript unsafe "$1.cssText = $2" js_set_cssText
  :: CSSRule -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.parentRule" js_get_parentRule
  :: CSSRule -> (IO (Nullable CSSRuleClass))
foreign import javascript unsafe "$1.parentStyleSheet" js_get_parentStyleSheet
  :: CSSRule -> (IO (Nullable CSSStyleSheetClass))
