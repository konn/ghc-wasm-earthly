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
module GHC.Wasm.Web.Generated.Range (
        Range, RangeClass, js_const_Range_START_TO_START,
        js_const_Range_START_TO_END, js_const_Range_END_TO_END,
        js_const_Range_END_TO_START, js_fun_setStart_Node_long_undefined,
        js_fun_setEnd_Node_long_undefined,
        js_fun_setStartBefore_Node_undefined,
        js_fun_setStartAfter_Node_undefined,
        js_fun_setEndBefore_Node_undefined,
        js_fun_setEndAfter_Node_undefined,
        js_fun_collapse_nullable_boolean_undefined,
        js_fun_selectNode_Node_undefined,
        js_fun_selectNodeContents_Node_undefined,
        js_fun_compareBoundaryPoints_short_Range_short,
        js_fun_deleteContents__undefined,
        js_fun_extractContents__DocumentFragment,
        js_fun_cloneContents__DocumentFragment,
        js_fun_insertNode_Node_undefined,
        js_fun_surroundContents_Node_undefined, js_fun_cloneRange__Range,
        js_fun_detach__undefined, js_fun_isPointInRange_Node_long_boolean,
        js_fun_comparePoint_Node_long_short,
        js_fun_intersectsNode_Node_boolean,
        js_fun_createContextualFragment_DOMString_DocumentFragment,
        js_fun_getClientRects__nullable_DOMRectList,
        js_fun_getBoundingClientRect__DOMRect,
        js_fun_getClientRectsAndTexts__ClientRectsAndTexts,
        js_get_startContainer, js_get_startOffset, js_get_endContainer,
        js_get_endOffset, js_get_collapsed, js_get_commonAncestorContainer
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ClientRectsAndTexts.Core
import GHC.Wasm.Web.Generated.DOMRect.Core
import GHC.Wasm.Web.Generated.DOMRectList.Core
import GHC.Wasm.Web.Generated.DocumentFragment.Core
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Generated.Range.Core
import GHC.Wasm.Web.Types
js_const_Range_START_TO_START :: Word16
js_const_Range_START_TO_START = 0
js_const_Range_START_TO_END :: Word16
js_const_Range_START_TO_END = 1
js_const_Range_END_TO_END :: Word16
js_const_Range_END_TO_END = 2
js_const_Range_END_TO_START :: Word16
js_const_Range_END_TO_START = 3
foreign import javascript unsafe "$1.setStart($2,$3)" js_fun_setStart_Node_long_undefined
  :: Range -> (Node -> (Word32 -> (IO ())))
foreign import javascript unsafe "$1.setEnd($2,$3)" js_fun_setEnd_Node_long_undefined
  :: Range -> (Node -> (Word32 -> (IO ())))
foreign import javascript unsafe "$1.setStartBefore($2)" js_fun_setStartBefore_Node_undefined
  :: Range -> (Node -> (IO ()))
foreign import javascript unsafe "$1.setStartAfter($2)" js_fun_setStartAfter_Node_undefined
  :: Range -> (Node -> (IO ()))
foreign import javascript unsafe "$1.setEndBefore($2)" js_fun_setEndBefore_Node_undefined
  :: Range -> (Node -> (IO ()))
foreign import javascript unsafe "$1.setEndAfter($2)" js_fun_setEndAfter_Node_undefined
  :: Range -> (Node -> (IO ()))
foreign import javascript unsafe "$1.collapse($2)" js_fun_collapse_nullable_boolean_undefined
  :: Range -> (Nullable (JSPrimClass Bool) -> (IO ()))
foreign import javascript unsafe "$1.selectNode($2)" js_fun_selectNode_Node_undefined
  :: Range -> (Node -> (IO ()))
foreign import javascript unsafe "$1.selectNodeContents($2)" js_fun_selectNodeContents_Node_undefined
  :: Range -> (Node -> (IO ()))
foreign import javascript unsafe "$1.compareBoundaryPoints($2,$3)" js_fun_compareBoundaryPoints_short_Range_short
  :: Range -> (Word16 -> (Range -> (IO Int16)))
foreign import javascript unsafe "$1.deleteContents()" js_fun_deleteContents__undefined
  :: Range -> (IO ())
foreign import javascript unsafe "$1.extractContents()" js_fun_extractContents__DocumentFragment
  :: Range -> (IO DocumentFragment)
foreign import javascript unsafe "$1.cloneContents()" js_fun_cloneContents__DocumentFragment
  :: Range -> (IO DocumentFragment)
foreign import javascript unsafe "$1.insertNode($2)" js_fun_insertNode_Node_undefined
  :: Range -> (Node -> (IO ()))
foreign import javascript unsafe "$1.surroundContents($2)" js_fun_surroundContents_Node_undefined
  :: Range -> (Node -> (IO ()))
foreign import javascript unsafe "$1.cloneRange()" js_fun_cloneRange__Range
  :: Range -> (IO Range)
foreign import javascript unsafe "$1.detach()" js_fun_detach__undefined
  :: Range -> (IO ())
foreign import javascript unsafe "$1.isPointInRange($2,$3)" js_fun_isPointInRange_Node_long_boolean
  :: Range -> (Node -> (Word32 -> (IO Bool)))
foreign import javascript unsafe "$1.comparePoint($2,$3)" js_fun_comparePoint_Node_long_short
  :: Range -> (Node -> (Word32 -> (IO Int16)))
foreign import javascript unsafe "$1.intersectsNode($2)" js_fun_intersectsNode_Node_boolean
  :: Range -> (Node -> (IO Bool))
foreign import javascript unsafe "$1.createContextualFragment($2)" js_fun_createContextualFragment_DOMString_DocumentFragment
  :: Range -> (DOMString -> (IO DocumentFragment))
foreign import javascript unsafe "$1.getClientRects()" js_fun_getClientRects__nullable_DOMRectList
  :: Range -> (IO (Nullable DOMRectListClass))
foreign import javascript unsafe "$1.getBoundingClientRect()" js_fun_getBoundingClientRect__DOMRect
  :: Range -> (IO DOMRect)
foreign import javascript unsafe "$1.getClientRectsAndTexts()" js_fun_getClientRectsAndTexts__ClientRectsAndTexts
  :: Range -> (IO ClientRectsAndTexts)
foreign import javascript unsafe "$1.startContainer" js_get_startContainer
  :: Range -> (IO Node)
foreign import javascript unsafe "$1.startOffset" js_get_startOffset
  :: Range -> (IO Word32)
foreign import javascript unsafe "$1.endContainer" js_get_endContainer
  :: Range -> (IO Node)
foreign import javascript unsafe "$1.endOffset" js_get_endOffset
  :: Range -> (IO Word32)
foreign import javascript unsafe "$1.collapsed" js_get_collapsed
  :: Range -> (IO Bool)
foreign import javascript unsafe "$1.commonAncestorContainer" js_get_commonAncestorContainer
  :: Range -> (IO Node)
