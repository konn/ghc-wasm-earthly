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
module GHC.Wasm.Web.Generated.Selection (
        Selection, SelectionClass, js_fun_getRangeAt_long_Range,
        js_fun_addRange_Range_undefined,
        js_fun_removeRange_Range_undefined,
        js_fun_removeAllRanges__undefined, js_fun_empty__undefined,
        js_fun_collapse_nullable_Node_nullable_long_undefined,
        js_fun_setPosition_nullable_Node_nullable_long_undefined,
        js_fun_collapseToStart__undefined, js_fun_collapseToEnd__undefined,
        js_fun_extend_Node_nullable_long_undefined,
        js_fun_setBaseAndExtent_Node_long_Node_long_undefined,
        js_fun_selectAllChildren_Node_undefined,
        js_fun_deleteFromDocument__undefined,
        js_fun_containsNode_Node_nullable_boolean_boolean,
        js_fun_modify_DOMString_DOMString_DOMString_undefined,
        js_fun_toStringWithFormat_DOMString_long_long_DOMString,
        js_fun_addSelectionListener_nsISelectionListener_undefined,
        js_fun_removeSelectionListener_nsISelectionListener_undefined,
        js_fun_GetRangesForInterval_Node_long_Node_long_boolean_sequence_Range,
        js_fun_scrollIntoView_short_boolean_short_short_undefined,
        js_fun_setColors_DOMString_DOMString_DOMString_DOMString_undefined,
        js_fun_resetColors__undefined, js_get_anchorNode,
        js_get_anchorOffset, js_get_focusNode, js_get_focusOffset,
        js_get_isCollapsed, js_get_rangeCount, js_get_type,
        js_get_interlinePosition, js_set_interlinePosition,
        js_get_caretBidiLevel, js_set_caretBidiLevel, js_get_selectionType
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Generated.NsISelectionListener.Core
import GHC.Wasm.Web.Generated.Range.Core
import GHC.Wasm.Web.Generated.Selection.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getRangeAt($2)" js_fun_getRangeAt_long_Range
  :: Selection -> (Word32 -> (IO Range))
foreign import javascript unsafe "$1.addRange($2)" js_fun_addRange_Range_undefined
  :: Selection -> (Range -> (IO ()))
foreign import javascript unsafe "$1.removeRange($2)" js_fun_removeRange_Range_undefined
  :: Selection -> (Range -> (IO ()))
foreign import javascript unsafe "$1.removeAllRanges()" js_fun_removeAllRanges__undefined
  :: Selection -> (IO ())
foreign import javascript unsafe "$1.empty()" js_fun_empty__undefined
  :: Selection -> (IO ())
foreign import javascript unsafe "$1.collapse($2,$3)" js_fun_collapse_nullable_Node_nullable_long_undefined
  :: Selection
     -> (Nullable NodeClass
         -> (Nullable (JSPrimClass Word32) -> (IO ())))
foreign import javascript unsafe "$1.setPosition($2,$3)" js_fun_setPosition_nullable_Node_nullable_long_undefined
  :: Selection
     -> (Nullable NodeClass
         -> (Nullable (JSPrimClass Word32) -> (IO ())))
foreign import javascript unsafe "$1.collapseToStart()" js_fun_collapseToStart__undefined
  :: Selection -> (IO ())
foreign import javascript unsafe "$1.collapseToEnd()" js_fun_collapseToEnd__undefined
  :: Selection -> (IO ())
foreign import javascript unsafe "$1.extend($2,$3)" js_fun_extend_Node_nullable_long_undefined
  :: Selection
     -> (Node -> (Nullable (JSPrimClass Word32) -> (IO ())))
foreign import javascript unsafe "$1.setBaseAndExtent($2,$3,$4,$5)" js_fun_setBaseAndExtent_Node_long_Node_long_undefined
  :: Selection -> (Node -> (Word32 -> (Node -> (Word32 -> (IO ())))))
foreign import javascript unsafe "$1.selectAllChildren($2)" js_fun_selectAllChildren_Node_undefined
  :: Selection -> (Node -> (IO ()))
foreign import javascript unsafe "$1.deleteFromDocument()" js_fun_deleteFromDocument__undefined
  :: Selection -> (IO ())
foreign import javascript unsafe "$1.containsNode($2,$3)" js_fun_containsNode_Node_nullable_boolean_boolean
  :: Selection
     -> (Node -> (Nullable (JSPrimClass Bool) -> (IO Bool)))
foreign import javascript unsafe "$1.modify($2,$3,$4)" js_fun_modify_DOMString_DOMString_DOMString_undefined
  :: Selection
     -> (DOMString -> (DOMString -> (DOMString -> (IO ()))))
foreign import javascript unsafe "$1.toStringWithFormat($2,$3,$4)" js_fun_toStringWithFormat_DOMString_long_long_DOMString
  :: Selection
     -> (DOMString -> (Word32 -> (Int32 -> (IO DOMString))))
foreign import javascript unsafe "$1.addSelectionListener($2)" js_fun_addSelectionListener_nsISelectionListener_undefined
  :: Selection -> (NsISelectionListener -> (IO ()))
foreign import javascript unsafe "$1.removeSelectionListener($2)" js_fun_removeSelectionListener_nsISelectionListener_undefined
  :: Selection -> (NsISelectionListener -> (IO ()))
foreign import javascript unsafe "$1.GetRangesForInterval($2,$3,$4,$5,$6)" js_fun_GetRangesForInterval_Node_long_Node_long_boolean_sequence_Range
  :: Selection
     -> (Node
         -> (Int32
             -> (Node -> (Int32 -> (Bool -> (IO (Sequence RangeClass)))))))
foreign import javascript unsafe "$1.scrollIntoView($2,$3,$4,$5)" js_fun_scrollIntoView_short_boolean_short_short_undefined
  :: Selection -> (Int16 -> (Bool -> (Int16 -> (Int16 -> (IO ())))))
foreign import javascript unsafe "$1.setColors($2,$3,$4,$5)" js_fun_setColors_DOMString_DOMString_DOMString_DOMString_undefined
  :: Selection
     -> (DOMString
         -> (DOMString -> (DOMString -> (DOMString -> (IO ())))))
foreign import javascript unsafe "$1.resetColors()" js_fun_resetColors__undefined
  :: Selection -> (IO ())
foreign import javascript unsafe "$1.anchorNode" js_get_anchorNode
  :: Selection -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.anchorOffset" js_get_anchorOffset
  :: Selection -> (IO Word32)
foreign import javascript unsafe "$1.focusNode" js_get_focusNode
  :: Selection -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.focusOffset" js_get_focusOffset
  :: Selection -> (IO Word32)
foreign import javascript unsafe "$1.isCollapsed" js_get_isCollapsed
  :: Selection -> (IO Bool)
foreign import javascript unsafe "$1.rangeCount" js_get_rangeCount
  :: Selection -> (IO Word32)
foreign import javascript unsafe "$1.type" js_get_type
  :: Selection -> (IO DOMString)
foreign import javascript unsafe "$1.interlinePosition" js_get_interlinePosition
  :: Selection -> (IO Bool)
foreign import javascript unsafe "$1.interlinePosition = $2" js_set_interlinePosition
  :: Selection -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.caretBidiLevel" js_get_caretBidiLevel
  :: Selection -> (IO (Nullable (JSPrimClass Int16)))
foreign import javascript unsafe "$1.caretBidiLevel = $2" js_set_caretBidiLevel
  :: Selection -> (Nullable (JSPrimClass Int16) -> (IO ()))
foreign import javascript unsafe "$1.selectionType" js_get_selectionType
  :: Selection -> (IO Int16)
