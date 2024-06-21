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
module GHC.Wasm.Web.Generated.Element (
        Element, ElementClass,
        js_fun_getAttributeNames__sequence_DOMString,
        js_fun_getAttribute_DOMString_nullable_DOMString,
        js_fun_getAttributeNS_nullable_DOMString_DOMString_nullable_DOMString,
        js_fun_toggleAttribute_DOMString_nullable_boolean_boolean,
        js_fun_setAttribute_DOMString_DOMString_undefined,
        js_fun_setAttributeNS_nullable_DOMString_DOMString_DOMString_undefined,
        js_fun_removeAttribute_DOMString_undefined,
        js_fun_removeAttributeNS_nullable_DOMString_DOMString_undefined,
        js_fun_hasAttribute_DOMString_boolean,
        js_fun_hasAttributeNS_nullable_DOMString_DOMString_boolean,
        js_fun_hasAttributes__boolean,
        js_fun_closest_DOMString_nullable_Element,
        js_fun_matches_DOMString_boolean,
        js_fun_webkitMatchesSelector_DOMString_boolean,
        js_fun_getElementsByTagName_DOMString_HTMLCollection,
        js_fun_getElementsByTagNameNS_nullable_DOMString_DOMString_HTMLCollection,
        js_fun_getElementsByClassName_DOMString_HTMLCollection,
        js_fun_getElementsWithGrid__sequence_Element,
        js_fun_insertAdjacentElement_DOMString_Element_nullable_Element,
        js_fun_insertAdjacentText_DOMString_DOMString_undefined,
        js_fun_setPointerCapture_long_undefined,
        js_fun_releasePointerCapture_long_undefined,
        js_fun_hasPointerCapture_long_boolean,
        js_fun_setCapture_nullable_boolean_undefined,
        js_fun_releaseCapture__undefined,
        js_fun_setCaptureAlways_nullable_boolean_undefined,
        js_fun_getAttributeNode_DOMString_nullable_Attr,
        js_fun_setAttributeNode_Attr_nullable_Attr,
        js_fun_removeAttributeNode_Attr_nullable_Attr,
        js_fun_getAttributeNodeNS_nullable_DOMString_DOMString_nullable_Attr,
        js_fun_setAttributeNodeNS_Attr_nullable_Attr,
        js_fun_scrollByNoFlush_long_long_boolean,
        js_fun_getAsFlexContainer__nullable_Flex,
        js_fun_getGridFragments__sequence_Grid,
        js_fun_getTransformToAncestor_Element_DOMMatrixReadOnly,
        js_fun_getTransformToParent__DOMMatrixReadOnly,
        js_fun_getTransformToViewport__DOMMatrixReadOnly,
        js_fun_getClientRects__DOMRectList,
        js_fun_getBoundingClientRect__DOMRect,
        js_fun_scrollIntoView_nullable_Union_boolean_ScrollIntoViewOptions_EndUnion_undefined,
        js_fun_scroll_double_double_undefined,
        js_fun_scroll_nullable_ScrollToOptions_undefined,
        js_fun_scrollTo_double_double_undefined,
        js_fun_scrollTo_nullable_ScrollToOptions_undefined,
        js_fun_scrollBy_double_double_undefined,
        js_fun_scrollBy_nullable_ScrollToOptions_undefined,
        js_fun_insertAdjacentHTML_DOMString_DOMString_undefined,
        js_fun_querySelector_DOMString_nullable_Element,
        js_fun_querySelectorAll_DOMString_NodeList,
        js_fun_attachShadow_ShadowRootInit_ShadowRoot,
        js_fun_requestFullscreen__undefined,
        js_fun_requestPointerLock__undefined,
        js_fun_before_Union_Node_DOMString_EndUnion_undefined,
        js_fun_after_Union_Node_DOMString_EndUnion_undefined,
        js_fun_replaceWith_Union_Node_DOMString_EndUnion_undefined,
        js_fun_remove__undefined,
        js_fun_prepend_Union_Node_DOMString_EndUnion_undefined,
        js_fun_append_Union_Node_DOMString_EndUnion_undefined,
        js_fun_replaceChildren_Union_Node_DOMString_EndUnion_undefined,
        js_fun_animate_nullable_object_nullable_Union_double_KeyframeAnimationOptions_EndUnion_Animation,
        js_fun_getAnimations_nullable_GetAnimationsOptions_sequence_Animation,
        js_fun_getBoxQuads_nullable_BoxQuadOptions_sequence_DOMQuad,
        js_fun_convertQuadFromNode_DOMQuad_GeometryNode_nullable_ConvertCoordinateOptions_DOMQuad,
        js_fun_convertRectFromNode_DOMRectReadOnly_GeometryNode_nullable_ConvertCoordinateOptions_DOMQuad,
        js_fun_convertPointFromNode_DOMPointInit_GeometryNode_nullable_ConvertCoordinateOptions_DOMPoint,
        js_get_namespaceURI, js_get_prefix, js_get_localName,
        js_get_tagName, js_get_id, js_set_id, js_get_className,
        js_set_className, js_get_classList, js_get_attributes,
        js_get_fontSizeInflation, js_get_scrollTop, js_set_scrollTop,
        js_get_scrollLeft, js_set_scrollLeft, js_get_scrollWidth,
        js_get_scrollHeight, js_get_clientTop, js_get_clientLeft,
        js_get_clientWidth, js_get_clientHeight, js_get_innerHTML,
        js_set_innerHTML, js_get_outerHTML, js_set_outerHTML,
        js_get_shadowRoot, js_get_openOrClosedShadowRoot,
        js_get_assignedSlot, js_get_slot, js_set_slot,
        js_get_previousElementSibling, js_get_nextElementSibling,
        js_get_children, js_get_firstElementChild, js_get_lastElementChild,
        js_get_childElementCount
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Animation.Core
import GHC.Wasm.Web.Generated.Attr.Core
import GHC.Wasm.Web.Generated.BoxQuadOptions.Core
import GHC.Wasm.Web.Generated.ConvertCoordinateOptions.Core
import GHC.Wasm.Web.Generated.DOMMatrixReadOnly.Core
import GHC.Wasm.Web.Generated.DOMPoint.Core
import GHC.Wasm.Web.Generated.DOMPointInit.Core
import GHC.Wasm.Web.Generated.DOMQuad.Core
import GHC.Wasm.Web.Generated.DOMRect.Core
import GHC.Wasm.Web.Generated.DOMRectList.Core
import GHC.Wasm.Web.Generated.DOMRectReadOnly.Core
import GHC.Wasm.Web.Generated.DOMTokenList.Core
import GHC.Wasm.Web.Generated.Element.Core
import GHC.Wasm.Web.Generated.Flex.Core
import GHC.Wasm.Web.Generated.GeometryNode.Core
import GHC.Wasm.Web.Generated.GetAnimationsOptions.Core
import GHC.Wasm.Web.Generated.Grid.Core
import GHC.Wasm.Web.Generated.HTMLCollection.Core
import GHC.Wasm.Web.Generated.HTMLSlotElement.Core
import GHC.Wasm.Web.Generated.KeyframeAnimationOptions.Core
import GHC.Wasm.Web.Generated.NamedNodeMap.Core
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Generated.NodeList.Core
import GHC.Wasm.Web.Generated.ScrollIntoViewOptions.Core
import GHC.Wasm.Web.Generated.ScrollToOptions.Core
import GHC.Wasm.Web.Generated.ShadowRoot.Core
import GHC.Wasm.Web.Generated.ShadowRootInit.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getAttributeNames()" js_fun_getAttributeNames__sequence_DOMString
  :: Element -> (IO (Sequence DOMStringClass))
foreign import javascript unsafe "$1.getAttribute($2)" js_fun_getAttribute_DOMString_nullable_DOMString
  :: Element -> (DOMString -> (IO (Nullable DOMStringClass)))
foreign import javascript unsafe "$1.getAttributeNS($2,$3)" js_fun_getAttributeNS_nullable_DOMString_DOMString_nullable_DOMString
  :: Element
     -> (Nullable DOMStringClass
         -> (DOMString -> (IO (Nullable DOMStringClass))))
foreign import javascript unsafe "$1.toggleAttribute($2,$3)" js_fun_toggleAttribute_DOMString_nullable_boolean_boolean
  :: Element
     -> (DOMString -> (Nullable (JSPrimClass Bool) -> (IO Bool)))
foreign import javascript unsafe "$1.setAttribute($2,$3)" js_fun_setAttribute_DOMString_DOMString_undefined
  :: Element -> (DOMString -> (DOMString -> (IO ())))
foreign import javascript unsafe "$1.setAttributeNS($2,$3,$4)" js_fun_setAttributeNS_nullable_DOMString_DOMString_DOMString_undefined
  :: Element
     -> (Nullable DOMStringClass
         -> (DOMString -> (DOMString -> (IO ()))))
foreign import javascript unsafe "$1.removeAttribute($2)" js_fun_removeAttribute_DOMString_undefined
  :: Element -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.removeAttributeNS($2,$3)" js_fun_removeAttributeNS_nullable_DOMString_DOMString_undefined
  :: Element -> (Nullable DOMStringClass -> (DOMString -> (IO ())))
foreign import javascript unsafe "$1.hasAttribute($2)" js_fun_hasAttribute_DOMString_boolean
  :: Element -> (DOMString -> (IO Bool))
foreign import javascript unsafe "$1.hasAttributeNS($2,$3)" js_fun_hasAttributeNS_nullable_DOMString_DOMString_boolean
  :: Element -> (Nullable DOMStringClass -> (DOMString -> (IO Bool)))
foreign import javascript unsafe "$1.hasAttributes()" js_fun_hasAttributes__boolean
  :: Element -> (IO Bool)
foreign import javascript unsafe "$1.closest($2)" js_fun_closest_DOMString_nullable_Element
  :: Element -> (DOMString -> (IO (Nullable ElementClass)))
foreign import javascript unsafe "$1.matches($2)" js_fun_matches_DOMString_boolean
  :: Element -> (DOMString -> (IO Bool))
foreign import javascript unsafe "$1.webkitMatchesSelector($2)" js_fun_webkitMatchesSelector_DOMString_boolean
  :: Element -> (DOMString -> (IO Bool))
foreign import javascript unsafe "$1.getElementsByTagName($2)" js_fun_getElementsByTagName_DOMString_HTMLCollection
  :: Element -> (DOMString -> (IO HTMLCollection))
foreign import javascript unsafe "$1.getElementsByTagNameNS($2,$3)" js_fun_getElementsByTagNameNS_nullable_DOMString_DOMString_HTMLCollection
  :: Element
     -> (Nullable DOMStringClass -> (DOMString -> (IO HTMLCollection)))
foreign import javascript unsafe "$1.getElementsByClassName($2)" js_fun_getElementsByClassName_DOMString_HTMLCollection
  :: Element -> (DOMString -> (IO HTMLCollection))
foreign import javascript unsafe "$1.getElementsWithGrid()" js_fun_getElementsWithGrid__sequence_Element
  :: Element -> (IO (Sequence ElementClass))
foreign import javascript unsafe "$1.insertAdjacentElement($2,$3)" js_fun_insertAdjacentElement_DOMString_Element_nullable_Element
  :: Element
     -> (DOMString -> (Element -> (IO (Nullable ElementClass))))
foreign import javascript unsafe "$1.insertAdjacentText($2,$3)" js_fun_insertAdjacentText_DOMString_DOMString_undefined
  :: Element -> (DOMString -> (DOMString -> (IO ())))
foreign import javascript unsafe "$1.setPointerCapture($2)" js_fun_setPointerCapture_long_undefined
  :: Element -> (Int32 -> (IO ()))
foreign import javascript unsafe "$1.releasePointerCapture($2)" js_fun_releasePointerCapture_long_undefined
  :: Element -> (Int32 -> (IO ()))
foreign import javascript unsafe "$1.hasPointerCapture($2)" js_fun_hasPointerCapture_long_boolean
  :: Element -> (Int32 -> (IO Bool))
foreign import javascript unsafe "$1.setCapture($2)" js_fun_setCapture_nullable_boolean_undefined
  :: Element -> (Nullable (JSPrimClass Bool) -> (IO ()))
foreign import javascript unsafe "$1.releaseCapture()" js_fun_releaseCapture__undefined
  :: Element -> (IO ())
foreign import javascript unsafe "$1.setCaptureAlways($2)" js_fun_setCaptureAlways_nullable_boolean_undefined
  :: Element -> (Nullable (JSPrimClass Bool) -> (IO ()))
foreign import javascript unsafe "$1.getAttributeNode($2)" js_fun_getAttributeNode_DOMString_nullable_Attr
  :: Element -> (DOMString -> (IO (Nullable AttrClass)))
foreign import javascript unsafe "$1.setAttributeNode($2)" js_fun_setAttributeNode_Attr_nullable_Attr
  :: Element -> (Attr -> (IO (Nullable AttrClass)))
foreign import javascript unsafe "$1.removeAttributeNode($2)" js_fun_removeAttributeNode_Attr_nullable_Attr
  :: Element -> (Attr -> (IO (Nullable AttrClass)))
foreign import javascript unsafe "$1.getAttributeNodeNS($2,$3)" js_fun_getAttributeNodeNS_nullable_DOMString_DOMString_nullable_Attr
  :: Element
     -> (Nullable DOMStringClass
         -> (DOMString -> (IO (Nullable AttrClass))))
foreign import javascript unsafe "$1.setAttributeNodeNS($2)" js_fun_setAttributeNodeNS_Attr_nullable_Attr
  :: Element -> (Attr -> (IO (Nullable AttrClass)))
foreign import javascript unsafe "$1.scrollByNoFlush($2,$3)" js_fun_scrollByNoFlush_long_long_boolean
  :: Element -> (Int32 -> (Int32 -> (IO Bool)))
foreign import javascript unsafe "$1.getAsFlexContainer()" js_fun_getAsFlexContainer__nullable_Flex
  :: Element -> (IO (Nullable FlexClass))
foreign import javascript unsafe "$1.getGridFragments()" js_fun_getGridFragments__sequence_Grid
  :: Element -> (IO (Sequence GridClass))
foreign import javascript unsafe "$1.getTransformToAncestor($2)" js_fun_getTransformToAncestor_Element_DOMMatrixReadOnly
  :: Element -> (Element -> (IO DOMMatrixReadOnly))
foreign import javascript unsafe "$1.getTransformToParent()" js_fun_getTransformToParent__DOMMatrixReadOnly
  :: Element -> (IO DOMMatrixReadOnly)
foreign import javascript unsafe "$1.getTransformToViewport()" js_fun_getTransformToViewport__DOMMatrixReadOnly
  :: Element -> (IO DOMMatrixReadOnly)
foreign import javascript unsafe "$1.getClientRects()" js_fun_getClientRects__DOMRectList
  :: Element -> (IO DOMRectList)
foreign import javascript unsafe "$1.getBoundingClientRect()" js_fun_getBoundingClientRect__DOMRect
  :: Element -> (IO DOMRect)
foreign import javascript unsafe "$1.scrollIntoView($2)" js_fun_scrollIntoView_nullable_Union_boolean_ScrollIntoViewOptions_EndUnion_undefined
  :: Element
     -> (Nullable (UnionClass '[JSPrimClass Bool,
                                ScrollIntoViewOptionsClass])
         -> (IO ()))
foreign import javascript unsafe "$1.scroll($2,$3)" js_fun_scroll_double_double_undefined
  :: Element -> (Double -> (Double -> (IO ())))
foreign import javascript unsafe "$1.scroll($2)" js_fun_scroll_nullable_ScrollToOptions_undefined
  :: Element -> (Nullable ScrollToOptionsClass -> (IO ()))
foreign import javascript unsafe "$1.scrollTo($2,$3)" js_fun_scrollTo_double_double_undefined
  :: Element -> (Double -> (Double -> (IO ())))
foreign import javascript unsafe "$1.scrollTo($2)" js_fun_scrollTo_nullable_ScrollToOptions_undefined
  :: Element -> (Nullable ScrollToOptionsClass -> (IO ()))
foreign import javascript unsafe "$1.scrollBy($2,$3)" js_fun_scrollBy_double_double_undefined
  :: Element -> (Double -> (Double -> (IO ())))
foreign import javascript unsafe "$1.scrollBy($2)" js_fun_scrollBy_nullable_ScrollToOptions_undefined
  :: Element -> (Nullable ScrollToOptionsClass -> (IO ()))
foreign import javascript unsafe "$1.insertAdjacentHTML($2,$3)" js_fun_insertAdjacentHTML_DOMString_DOMString_undefined
  :: Element -> (DOMString -> (DOMString -> (IO ())))
foreign import javascript unsafe "$1.querySelector($2)" js_fun_querySelector_DOMString_nullable_Element
  :: Element -> (DOMString -> (IO (Nullable ElementClass)))
foreign import javascript unsafe "$1.querySelectorAll($2)" js_fun_querySelectorAll_DOMString_NodeList
  :: Element -> (DOMString -> (IO NodeList))
foreign import javascript unsafe "$1.attachShadow($2)" js_fun_attachShadow_ShadowRootInit_ShadowRoot
  :: Element -> (ShadowRootInit -> (IO ShadowRoot))
foreign import javascript unsafe "$1.requestFullscreen()" js_fun_requestFullscreen__undefined
  :: Element -> (IO ())
foreign import javascript unsafe "$1.requestPointerLock()" js_fun_requestPointerLock__undefined
  :: Element -> (IO ())
foreign import javascript unsafe "$1.before(... $2)" js_fun_before_Union_Node_DOMString_EndUnion_undefined
  :: Element
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.after(... $2)" js_fun_after_Union_Node_DOMString_EndUnion_undefined
  :: Element
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.replaceWith(... $2)" js_fun_replaceWith_Union_Node_DOMString_EndUnion_undefined
  :: Element
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.remove()" js_fun_remove__undefined
  :: Element -> (IO ())
foreign import javascript unsafe "$1.prepend(... $2)" js_fun_prepend_Union_Node_DOMString_EndUnion_undefined
  :: Element
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.append(... $2)" js_fun_append_Union_Node_DOMString_EndUnion_undefined
  :: Element
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.replaceChildren(... $2)" js_fun_replaceChildren_Union_Node_DOMString_EndUnion_undefined
  :: Element
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.animate($2,$3)" js_fun_animate_nullable_object_nullable_Union_double_KeyframeAnimationOptions_EndUnion_Animation
  :: Element
     -> (Nullable AnyClass
         -> (Nullable (UnionClass '[JSPrimClass Double,
                                    KeyframeAnimationOptionsClass])
             -> (IO Animation)))
foreign import javascript unsafe "$1.getAnimations($2)" js_fun_getAnimations_nullable_GetAnimationsOptions_sequence_Animation
  :: Element
     -> (Nullable GetAnimationsOptionsClass
         -> (IO (Sequence AnimationClass)))
foreign import javascript unsafe "$1.getBoxQuads($2)" js_fun_getBoxQuads_nullable_BoxQuadOptions_sequence_DOMQuad
  :: Element
     -> (Nullable BoxQuadOptionsClass -> (IO (Sequence DOMQuadClass)))
foreign import javascript unsafe "$1.convertQuadFromNode($2,$3,$4)" js_fun_convertQuadFromNode_DOMQuad_GeometryNode_nullable_ConvertCoordinateOptions_DOMQuad
  :: Element
     -> (DOMQuad
         -> (GeometryNode
             -> (Nullable ConvertCoordinateOptionsClass -> (IO DOMQuad))))
foreign import javascript unsafe "$1.convertRectFromNode($2,$3,$4)" js_fun_convertRectFromNode_DOMRectReadOnly_GeometryNode_nullable_ConvertCoordinateOptions_DOMQuad
  :: Element
     -> (DOMRectReadOnly
         -> (GeometryNode
             -> (Nullable ConvertCoordinateOptionsClass -> (IO DOMQuad))))
foreign import javascript unsafe "$1.convertPointFromNode($2,$3,$4)" js_fun_convertPointFromNode_DOMPointInit_GeometryNode_nullable_ConvertCoordinateOptions_DOMPoint
  :: Element
     -> (DOMPointInit
         -> (GeometryNode
             -> (Nullable ConvertCoordinateOptionsClass -> (IO DOMPoint))))
foreign import javascript unsafe "$1.namespaceURI" js_get_namespaceURI
  :: Element -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.prefix" js_get_prefix
  :: Element -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.localName" js_get_localName
  :: Element -> (IO DOMString)
foreign import javascript unsafe "$1.tagName" js_get_tagName
  :: Element -> (IO DOMString)
foreign import javascript unsafe "$1.id" js_get_id
  :: Element -> (IO DOMString)
foreign import javascript unsafe "$1.id = $2" js_set_id
  :: Element -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.className" js_get_className
  :: Element -> (IO DOMString)
foreign import javascript unsafe "$1.className = $2" js_set_className
  :: Element -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.classList" js_get_classList
  :: Element -> (IO DOMTokenList)
foreign import javascript unsafe "$1.attributes" js_get_attributes
  :: Element -> (IO NamedNodeMap)
foreign import javascript unsafe "$1.fontSizeInflation" js_get_fontSizeInflation
  :: Element -> (IO Float)
foreign import javascript unsafe "$1.scrollTop" js_get_scrollTop
  :: Element -> (IO Int32)
foreign import javascript unsafe "$1.scrollTop = $2" js_set_scrollTop
  :: Element -> (Int32 -> (IO ()))
foreign import javascript unsafe "$1.scrollLeft" js_get_scrollLeft
  :: Element -> (IO Int32)
foreign import javascript unsafe "$1.scrollLeft = $2" js_set_scrollLeft
  :: Element -> (Int32 -> (IO ()))
foreign import javascript unsafe "$1.scrollWidth" js_get_scrollWidth
  :: Element -> (IO Int32)
foreign import javascript unsafe "$1.scrollHeight" js_get_scrollHeight
  :: Element -> (IO Int32)
foreign import javascript unsafe "$1.clientTop" js_get_clientTop
  :: Element -> (IO Int32)
foreign import javascript unsafe "$1.clientLeft" js_get_clientLeft
  :: Element -> (IO Int32)
foreign import javascript unsafe "$1.clientWidth" js_get_clientWidth
  :: Element -> (IO Int32)
foreign import javascript unsafe "$1.clientHeight" js_get_clientHeight
  :: Element -> (IO Int32)
foreign import javascript unsafe "$1.innerHTML" js_get_innerHTML
  :: Element -> (IO DOMString)
foreign import javascript unsafe "$1.innerHTML = $2" js_set_innerHTML
  :: Element -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.outerHTML" js_get_outerHTML
  :: Element -> (IO DOMString)
foreign import javascript unsafe "$1.outerHTML = $2" js_set_outerHTML
  :: Element -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.shadowRoot" js_get_shadowRoot
  :: Element -> (IO (Nullable ShadowRootClass))
foreign import javascript unsafe "$1.openOrClosedShadowRoot" js_get_openOrClosedShadowRoot
  :: Element -> (IO (Nullable ShadowRootClass))
foreign import javascript unsafe "$1.assignedSlot" js_get_assignedSlot
  :: Element -> (IO (Nullable HTMLSlotElementClass))
foreign import javascript unsafe "$1.slot" js_get_slot
  :: Element -> (IO DOMString)
foreign import javascript unsafe "$1.slot = $2" js_set_slot
  :: Element -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.previousElementSibling" js_get_previousElementSibling
  :: Element -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.nextElementSibling" js_get_nextElementSibling
  :: Element -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.children" js_get_children
  :: Element -> (IO HTMLCollection)
foreign import javascript unsafe "$1.firstElementChild" js_get_firstElementChild
  :: Element -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.lastElementChild" js_get_lastElementChild
  :: Element -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.childElementCount" js_get_childElementCount
  :: Element -> (IO Word32)
