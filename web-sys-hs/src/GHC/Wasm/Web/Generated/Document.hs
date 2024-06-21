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
module GHC.Wasm.Web.Generated.Document (
        Document, DocumentClass,
        js_fun_getElementsByTagName_DOMString_HTMLCollection,
        js_fun_getElementsByTagNameNS_nullable_DOMString_DOMString_HTMLCollection,
        js_fun_getElementsByClassName_DOMString_HTMLCollection,
        js_fun_getElementById_DOMString_nullable_Element,
        js_fun_createElement_DOMString_nullable_Union_ElementCreationOptions_DOMString_EndUnion_Element,
        js_fun_createElementNS_nullable_DOMString_DOMString_nullable_Union_ElementCreationOptions_DOMString_EndUnion_Element,
        js_fun_createDocumentFragment__DocumentFragment,
        js_fun_createTextNode_DOMString_Text,
        js_fun_createComment_DOMString_Comment,
        js_fun_createProcessingInstruction_DOMString_DOMString_ProcessingInstruction,
        js_fun_importNode_Node_nullable_boolean_Node,
        js_fun_adoptNode_Node_Node, js_fun_createEvent_DOMString_Event,
        js_fun_createRange__Range,
        js_fun_createNodeIterator_Node_nullable_long_nullable_nullable_NodeFilter_NodeIterator,
        js_fun_createTreeWalker_Node_nullable_long_nullable_nullable_NodeFilter_TreeWalker,
        js_fun_createCDATASection_DOMString_CDATASection,
        js_fun_createAttribute_DOMString_Attr,
        js_fun_createAttributeNS_nullable_DOMString_DOMString_Attr,
        js_fun_startViewTransition_nullable_nullable_UpdateCallback_ViewTransition,
        js_fun_getElementsByName_DOMString_NodeList,
        js_fun_hasFocus__boolean, js_fun_releaseCapture__undefined,
        js_fun_exitFullscreen__undefined,
        js_fun_exitPointerLock__undefined,
        js_fun_enableStyleSheetsForSet_nullable_DOMString_undefined,
        js_fun_caretPositionFromPoint_float_float_nullable_CaretPosition,
        js_fun_querySelector_DOMString_nullable_Element,
        js_fun_querySelectorAll_DOMString_NodeList,
        js_fun_getAnimations__sequence_Animation,
        js_fun_insertAnonymousContent_Element_AnonymousContent,
        js_fun_removeAnonymousContent_AnonymousContent_undefined,
        js_fun_getSelection__nullable_Selection,
        js_fun_notifyUserGestureActivation__undefined,
        js_fun_createExpression_DOMString_nullable_nullable_XPathNSResolver_XPathExpression,
        js_fun_createNSResolver_Node_Node,
        js_fun_evaluate_DOMString_Node_nullable_nullable_XPathNSResolver_nullable_short_nullable_nullable_object_XPathResult,
        js_fun_prepend_Union_Node_DOMString_EndUnion_undefined,
        js_fun_append_Union_Node_DOMString_EndUnion_undefined,
        js_fun_replaceChildren_Union_Node_DOMString_EndUnion_undefined,
        js_fun_getBoxQuads_nullable_BoxQuadOptions_sequence_DOMQuad,
        js_fun_convertQuadFromNode_DOMQuad_GeometryNode_nullable_ConvertCoordinateOptions_DOMQuad,
        js_fun_convertRectFromNode_DOMRectReadOnly_GeometryNode_nullable_ConvertCoordinateOptions_DOMQuad,
        js_fun_convertPointFromNode_DOMPointInit_GeometryNode_nullable_ConvertCoordinateOptions_DOMPoint,
        js_fun_elementFromPoint_float_float_nullable_Element,
        js_fun_elementsFromPoint_float_float_sequence_Element,
        js_get_implementation, js_get_URL, js_get_documentURI,
        js_get_compatMode, js_get_characterSet, js_get_charset,
        js_get_inputEncoding, js_get_contentType, js_get_doctype,
        js_get_documentElement, js_get_location, js_get_referrer,
        js_get_lastModified, js_get_readyState, js_get_title, js_set_title,
        js_get_dir, js_set_dir, js_get_body, js_set_body, js_get_head,
        js_get_images, js_get_embeds, js_get_plugins, js_get_links,
        js_get_forms, js_get_scripts, js_get_defaultView,
        js_get_onreadystatechange, js_set_onreadystatechange,
        js_get_onbeforescriptexecute, js_set_onbeforescriptexecute,
        js_get_onafterscriptexecute, js_set_onafterscriptexecute,
        js_get_onselectionchange, js_set_onselectionchange,
        js_get_currentScript, js_get_documentURIObject,
        js_get_referrerPolicy, js_get_anchors, js_get_applets,
        js_get_fullscreen, js_get_fullscreenEnabled,
        js_get_onfullscreenchange, js_set_onfullscreenchange,
        js_get_onfullscreenerror, js_set_onfullscreenerror,
        js_get_onpointerlockchange, js_set_onpointerlockchange,
        js_get_onpointerlockerror, js_set_onpointerlockerror,
        js_get_hidden, js_get_visibilityState, js_get_onvisibilitychange,
        js_set_onvisibilitychange, js_get_selectedStyleSheetSet,
        js_set_selectedStyleSheetSet, js_get_lastStyleSheetSet,
        js_get_preferredStyleSheetSet, js_get_styleSheetSets,
        js_get_scrollingElement, js_get_timeline, js_get_rootElement,
        js_get_isSrcdocDocument, js_get_sandboxFlagsAsString,
        js_get_userHasInteracted, js_get_documentFlashClassification,
        js_get_onabort, js_set_onabort, js_get_onblur, js_set_onblur,
        js_get_onfocus, js_set_onfocus, js_get_onauxclick,
        js_set_onauxclick, js_get_oncanplay, js_set_oncanplay,
        js_get_oncanplaythrough, js_set_oncanplaythrough, js_get_onchange,
        js_set_onchange, js_get_onclick, js_set_onclick, js_get_onclose,
        js_set_onclose, js_get_oncontextmenu, js_set_oncontextmenu,
        js_get_ondblclick, js_set_ondblclick, js_get_ondrag, js_set_ondrag,
        js_get_ondragend, js_set_ondragend, js_get_ondragenter,
        js_set_ondragenter, js_get_ondragexit, js_set_ondragexit,
        js_get_ondragleave, js_set_ondragleave, js_get_ondragover,
        js_set_ondragover, js_get_ondragstart, js_set_ondragstart,
        js_get_ondrop, js_set_ondrop, js_get_ondurationchange,
        js_set_ondurationchange, js_get_onemptied, js_set_onemptied,
        js_get_onended, js_set_onended, js_get_oninput, js_set_oninput,
        js_get_oninvalid, js_set_oninvalid, js_get_onkeydown,
        js_set_onkeydown, js_get_onkeypress, js_set_onkeypress,
        js_get_onkeyup, js_set_onkeyup, js_get_onload, js_set_onload,
        js_get_onloadeddata, js_set_onloadeddata, js_get_onloadedmetadata,
        js_set_onloadedmetadata, js_get_onloadend, js_set_onloadend,
        js_get_onloadstart, js_set_onloadstart, js_get_onmousedown,
        js_set_onmousedown, js_get_onmouseenter, js_set_onmouseenter,
        js_get_onmouseleave, js_set_onmouseleave, js_get_onmousemove,
        js_set_onmousemove, js_get_onmouseout, js_set_onmouseout,
        js_get_onmouseover, js_set_onmouseover, js_get_onmouseup,
        js_set_onmouseup, js_get_onwheel, js_set_onwheel, js_get_onpause,
        js_set_onpause, js_get_onplay, js_set_onplay, js_get_onplaying,
        js_set_onplaying, js_get_onprogress, js_set_onprogress,
        js_get_onratechange, js_set_onratechange, js_get_onreset,
        js_set_onreset, js_get_onresize, js_set_onresize, js_get_onscroll,
        js_set_onscroll, js_get_onseeked, js_set_onseeked,
        js_get_onseeking, js_set_onseeking, js_get_onselect,
        js_set_onselect, js_get_onshow, js_set_onshow, js_get_onstalled,
        js_set_onstalled, js_get_onsubmit, js_set_onsubmit,
        js_get_onsuspend, js_set_onsuspend, js_get_ontimeupdate,
        js_set_ontimeupdate, js_get_onvolumechange, js_set_onvolumechange,
        js_get_onwaiting, js_set_onwaiting, js_get_onselectstart,
        js_set_onselectstart, js_get_ontoggle, js_set_ontoggle,
        js_get_onpointercancel, js_set_onpointercancel,
        js_get_onpointerdown, js_set_onpointerdown, js_get_onpointerup,
        js_set_onpointerup, js_get_onpointermove, js_set_onpointermove,
        js_get_onpointerout, js_set_onpointerout, js_get_onpointerover,
        js_set_onpointerover, js_get_onpointerenter, js_set_onpointerenter,
        js_get_onpointerleave, js_set_onpointerleave,
        js_get_ongotpointercapture, js_set_ongotpointercapture,
        js_get_onlostpointercapture, js_set_onlostpointercapture,
        js_get_onanimationcancel, js_set_onanimationcancel,
        js_get_onanimationend, js_set_onanimationend,
        js_get_onanimationiteration, js_set_onanimationiteration,
        js_get_onanimationstart, js_set_onanimationstart,
        js_get_ontransitioncancel, js_set_ontransitioncancel,
        js_get_ontransitionend, js_set_ontransitionend,
        js_get_ontransitionrun, js_set_ontransitionrun,
        js_get_ontransitionstart, js_set_ontransitionstart,
        js_get_onwebkitanimationend, js_set_onwebkitanimationend,
        js_get_onwebkitanimationiteration,
        js_set_onwebkitanimationiteration, js_get_onwebkitanimationstart,
        js_set_onwebkitanimationstart, js_get_onwebkittransitionend,
        js_set_onwebkittransitionend, js_get_oncopy, js_set_oncopy,
        js_get_oncut, js_set_oncut, js_get_onpaste, js_set_onpaste,
        js_get_ontouchstart, js_set_ontouchstart, js_get_ontouchend,
        js_set_ontouchend, js_get_ontouchmove, js_set_ontouchmove,
        js_get_ontouchcancel, js_set_ontouchcancel, js_get_children,
        js_get_firstElementChild, js_get_lastElementChild,
        js_get_childElementCount, js_get_onerror, js_set_onerror,
        js_get_fonts, js_get_activeElement, js_get_styleSheets,
        js_get_pointerLockElement, js_get_fullscreenElement
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Animation.Core
import GHC.Wasm.Web.Generated.AnonymousContent.Core
import GHC.Wasm.Web.Generated.Attr.Core
import GHC.Wasm.Web.Generated.BoxQuadOptions.Core
import GHC.Wasm.Web.Generated.CDATASection.Core
import GHC.Wasm.Web.Generated.CaretPosition.Core
import GHC.Wasm.Web.Generated.Comment.Core
import GHC.Wasm.Web.Generated.ConvertCoordinateOptions.Core
import GHC.Wasm.Web.Generated.DOMImplementation.Core
import GHC.Wasm.Web.Generated.DOMPoint.Core
import GHC.Wasm.Web.Generated.DOMPointInit.Core
import GHC.Wasm.Web.Generated.DOMQuad.Core
import GHC.Wasm.Web.Generated.DOMRectReadOnly.Core
import GHC.Wasm.Web.Generated.DOMStringList.Core
import GHC.Wasm.Web.Generated.Document.Core
import GHC.Wasm.Web.Generated.DocumentFragment.Core
import GHC.Wasm.Web.Generated.DocumentTimeline.Core
import GHC.Wasm.Web.Generated.DocumentType.Core
import GHC.Wasm.Web.Generated.Element.Core
import GHC.Wasm.Web.Generated.ElementCreationOptions.Core
import GHC.Wasm.Web.Generated.Event.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.FlashClassification.Core
import GHC.Wasm.Web.Generated.FontFaceSet.Core
import GHC.Wasm.Web.Generated.GeometryNode.Core
import GHC.Wasm.Web.Generated.HTMLCollection.Core
import GHC.Wasm.Web.Generated.HTMLElement.Core
import GHC.Wasm.Web.Generated.HTMLHeadElement.Core
import GHC.Wasm.Web.Generated.Location.Core
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Generated.NodeFilter.Core
import GHC.Wasm.Web.Generated.NodeIterator.Core
import GHC.Wasm.Web.Generated.NodeList.Core
import GHC.Wasm.Web.Generated.ProcessingInstruction.Core
import GHC.Wasm.Web.Generated.Range.Core
import GHC.Wasm.Web.Generated.SVGSVGElement.Core
import GHC.Wasm.Web.Generated.Selection.Core
import GHC.Wasm.Web.Generated.StyleSheetList.Core
import GHC.Wasm.Web.Generated.Text.Core
import GHC.Wasm.Web.Generated.TreeWalker.Core
import GHC.Wasm.Web.Generated.URI.Core
import GHC.Wasm.Web.Generated.UpdateCallback.Core
import GHC.Wasm.Web.Generated.ViewTransition.Core
import GHC.Wasm.Web.Generated.VisibilityState.Core
import GHC.Wasm.Web.Generated.WindowProxy.Core
import GHC.Wasm.Web.Generated.XPathExpression.Core
import GHC.Wasm.Web.Generated.XPathNSResolver.Core
import GHC.Wasm.Web.Generated.XPathResult.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getElementsByTagName($2)" js_fun_getElementsByTagName_DOMString_HTMLCollection
  :: Document -> (DOMString -> (IO HTMLCollection))
foreign import javascript unsafe "$1.getElementsByTagNameNS($2,$3)" js_fun_getElementsByTagNameNS_nullable_DOMString_DOMString_HTMLCollection
  :: Document
     -> (Nullable DOMStringClass -> (DOMString -> (IO HTMLCollection)))
foreign import javascript unsafe "$1.getElementsByClassName($2)" js_fun_getElementsByClassName_DOMString_HTMLCollection
  :: Document -> (DOMString -> (IO HTMLCollection))
foreign import javascript unsafe "$1.getElementById($2)" js_fun_getElementById_DOMString_nullable_Element
  :: Document -> (DOMString -> (IO (Nullable ElementClass)))
foreign import javascript unsafe "$1.createElement($2,$3)" js_fun_createElement_DOMString_nullable_Union_ElementCreationOptions_DOMString_EndUnion_Element
  :: Document
     -> (DOMString
         -> (Nullable (UnionClass '[ElementCreationOptionsClass,
                                    DOMStringClass])
             -> (IO Element)))
foreign import javascript unsafe "$1.createElementNS($2,$3,$4)" js_fun_createElementNS_nullable_DOMString_DOMString_nullable_Union_ElementCreationOptions_DOMString_EndUnion_Element
  :: Document
     -> (Nullable DOMStringClass
         -> (DOMString
             -> (Nullable (UnionClass '[ElementCreationOptionsClass,
                                        DOMStringClass])
                 -> (IO Element))))
foreign import javascript unsafe "$1.createDocumentFragment()" js_fun_createDocumentFragment__DocumentFragment
  :: Document -> (IO DocumentFragment)
foreign import javascript unsafe "$1.createTextNode($2)" js_fun_createTextNode_DOMString_Text
  :: Document -> (DOMString -> (IO Text))
foreign import javascript unsafe "$1.createComment($2)" js_fun_createComment_DOMString_Comment
  :: Document -> (DOMString -> (IO Comment))
foreign import javascript unsafe "$1.createProcessingInstruction($2,$3)" js_fun_createProcessingInstruction_DOMString_DOMString_ProcessingInstruction
  :: Document
     -> (DOMString -> (DOMString -> (IO ProcessingInstruction)))
foreign import javascript unsafe "$1.importNode($2,$3)" js_fun_importNode_Node_nullable_boolean_Node
  :: Document -> (Node -> (Nullable (JSPrimClass Bool) -> (IO Node)))
foreign import javascript unsafe "$1.adoptNode($2)" js_fun_adoptNode_Node_Node
  :: Document -> (Node -> (IO Node))
foreign import javascript unsafe "$1.createEvent($2)" js_fun_createEvent_DOMString_Event
  :: Document -> (DOMString -> (IO Event))
foreign import javascript unsafe "$1.createRange()" js_fun_createRange__Range
  :: Document -> (IO Range)
foreign import javascript unsafe "$1.createNodeIterator($2,$3,$4)" js_fun_createNodeIterator_Node_nullable_long_nullable_nullable_NodeFilter_NodeIterator
  :: Document
     -> (Node
         -> (Nullable (JSPrimClass Word32)
             -> (Nullable (NullableClass NodeFilterClass)
                 -> (IO NodeIterator))))
foreign import javascript unsafe "$1.createTreeWalker($2,$3,$4)" js_fun_createTreeWalker_Node_nullable_long_nullable_nullable_NodeFilter_TreeWalker
  :: Document
     -> (Node
         -> (Nullable (JSPrimClass Word32)
             -> (Nullable (NullableClass NodeFilterClass) -> (IO TreeWalker))))
foreign import javascript unsafe "$1.createCDATASection($2)" js_fun_createCDATASection_DOMString_CDATASection
  :: Document -> (DOMString -> (IO CDATASection))
foreign import javascript unsafe "$1.createAttribute($2)" js_fun_createAttribute_DOMString_Attr
  :: Document -> (DOMString -> (IO Attr))
foreign import javascript unsafe "$1.createAttributeNS($2,$3)" js_fun_createAttributeNS_nullable_DOMString_DOMString_Attr
  :: Document
     -> (Nullable DOMStringClass -> (DOMString -> (IO Attr)))
foreign import javascript unsafe "$1.startViewTransition($2)" js_fun_startViewTransition_nullable_nullable_UpdateCallback_ViewTransition
  :: Document
     -> (Nullable (NullableClass UpdateCallbackClass)
         -> (IO ViewTransition))
foreign import javascript unsafe "$1.getElementsByName($2)" js_fun_getElementsByName_DOMString_NodeList
  :: Document -> (DOMString -> (IO NodeList))
foreign import javascript unsafe "$1.hasFocus()" js_fun_hasFocus__boolean
  :: Document -> (IO Bool)
foreign import javascript unsafe "$1.releaseCapture()" js_fun_releaseCapture__undefined
  :: Document -> (IO ())
foreign import javascript unsafe "$1.exitFullscreen()" js_fun_exitFullscreen__undefined
  :: Document -> (IO ())
foreign import javascript unsafe "$1.exitPointerLock()" js_fun_exitPointerLock__undefined
  :: Document -> (IO ())
foreign import javascript unsafe "$1.enableStyleSheetsForSet($2)" js_fun_enableStyleSheetsForSet_nullable_DOMString_undefined
  :: Document -> (Nullable DOMStringClass -> (IO ()))
foreign import javascript unsafe "$1.caretPositionFromPoint($2,$3)" js_fun_caretPositionFromPoint_float_float_nullable_CaretPosition
  :: Document
     -> (Float -> (Float -> (IO (Nullable CaretPositionClass))))
foreign import javascript unsafe "$1.querySelector($2)" js_fun_querySelector_DOMString_nullable_Element
  :: Document -> (DOMString -> (IO (Nullable ElementClass)))
foreign import javascript unsafe "$1.querySelectorAll($2)" js_fun_querySelectorAll_DOMString_NodeList
  :: Document -> (DOMString -> (IO NodeList))
foreign import javascript unsafe "$1.getAnimations()" js_fun_getAnimations__sequence_Animation
  :: Document -> (IO (Sequence AnimationClass))
foreign import javascript unsafe "$1.insertAnonymousContent($2)" js_fun_insertAnonymousContent_Element_AnonymousContent
  :: Document -> (Element -> (IO AnonymousContent))
foreign import javascript unsafe "$1.removeAnonymousContent($2)" js_fun_removeAnonymousContent_AnonymousContent_undefined
  :: Document -> (AnonymousContent -> (IO ()))
foreign import javascript unsafe "$1.getSelection()" js_fun_getSelection__nullable_Selection
  :: Document -> (IO (Nullable SelectionClass))
foreign import javascript unsafe "$1.notifyUserGestureActivation()" js_fun_notifyUserGestureActivation__undefined
  :: Document -> (IO ())
foreign import javascript unsafe "$1.createExpression($2,$3)" js_fun_createExpression_DOMString_nullable_nullable_XPathNSResolver_XPathExpression
  :: Document
     -> (DOMString
         -> (Nullable (NullableClass XPathNSResolverClass)
             -> (IO XPathExpression)))
foreign import javascript unsafe "$1.createNSResolver($2)" js_fun_createNSResolver_Node_Node
  :: Document -> (Node -> (IO Node))
foreign import javascript unsafe "$1.evaluate($2,$3,$4,$5,$6)" js_fun_evaluate_DOMString_Node_nullable_nullable_XPathNSResolver_nullable_short_nullable_nullable_object_XPathResult
  :: Document
     -> (DOMString
         -> (Node
             -> (Nullable (NullableClass XPathNSResolverClass)
                 -> (Nullable (JSPrimClass Word16)
                     -> (Nullable (NullableClass AnyClass) -> (IO XPathResult))))))
foreign import javascript unsafe "$1.prepend(... $2)" js_fun_prepend_Union_Node_DOMString_EndUnion_undefined
  :: Document
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.append(... $2)" js_fun_append_Union_Node_DOMString_EndUnion_undefined
  :: Document
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.replaceChildren(... $2)" js_fun_replaceChildren_Union_Node_DOMString_EndUnion_undefined
  :: Document
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.getBoxQuads($2)" js_fun_getBoxQuads_nullable_BoxQuadOptions_sequence_DOMQuad
  :: Document
     -> (Nullable BoxQuadOptionsClass -> (IO (Sequence DOMQuadClass)))
foreign import javascript unsafe "$1.convertQuadFromNode($2,$3,$4)" js_fun_convertQuadFromNode_DOMQuad_GeometryNode_nullable_ConvertCoordinateOptions_DOMQuad
  :: Document
     -> (DOMQuad
         -> (GeometryNode
             -> (Nullable ConvertCoordinateOptionsClass -> (IO DOMQuad))))
foreign import javascript unsafe "$1.convertRectFromNode($2,$3,$4)" js_fun_convertRectFromNode_DOMRectReadOnly_GeometryNode_nullable_ConvertCoordinateOptions_DOMQuad
  :: Document
     -> (DOMRectReadOnly
         -> (GeometryNode
             -> (Nullable ConvertCoordinateOptionsClass -> (IO DOMQuad))))
foreign import javascript unsafe "$1.convertPointFromNode($2,$3,$4)" js_fun_convertPointFromNode_DOMPointInit_GeometryNode_nullable_ConvertCoordinateOptions_DOMPoint
  :: Document
     -> (DOMPointInit
         -> (GeometryNode
             -> (Nullable ConvertCoordinateOptionsClass -> (IO DOMPoint))))
foreign import javascript unsafe "$1.elementFromPoint($2,$3)" js_fun_elementFromPoint_float_float_nullable_Element
  :: Document -> (Float -> (Float -> (IO (Nullable ElementClass))))
foreign import javascript unsafe "$1.elementsFromPoint($2,$3)" js_fun_elementsFromPoint_float_float_sequence_Element
  :: Document -> (Float -> (Float -> (IO (Sequence ElementClass))))
foreign import javascript unsafe "$1.implementation" js_get_implementation
  :: Document -> (IO DOMImplementation)
foreign import javascript unsafe "$1.URL" js_get_URL
  :: Document -> (IO DOMString)
foreign import javascript unsafe "$1.documentURI" js_get_documentURI
  :: Document -> (IO DOMString)
foreign import javascript unsafe "$1.compatMode" js_get_compatMode
  :: Document -> (IO DOMString)
foreign import javascript unsafe "$1.characterSet" js_get_characterSet
  :: Document -> (IO DOMString)
foreign import javascript unsafe "$1.charset" js_get_charset
  :: Document -> (IO DOMString)
foreign import javascript unsafe "$1.inputEncoding" js_get_inputEncoding
  :: Document -> (IO DOMString)
foreign import javascript unsafe "$1.contentType" js_get_contentType
  :: Document -> (IO DOMString)
foreign import javascript unsafe "$1.doctype" js_get_doctype
  :: Document -> (IO (Nullable DocumentTypeClass))
foreign import javascript unsafe "$1.documentElement" js_get_documentElement
  :: Document -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.location" js_get_location
  :: Document -> (IO (Nullable LocationClass))
foreign import javascript unsafe "$1.referrer" js_get_referrer
  :: Document -> (IO DOMString)
foreign import javascript unsafe "$1.lastModified" js_get_lastModified
  :: Document -> (IO DOMString)
foreign import javascript unsafe "$1.readyState" js_get_readyState
  :: Document -> (IO DOMString)
foreign import javascript unsafe "$1.title" js_get_title
  :: Document -> (IO DOMString)
foreign import javascript unsafe "$1.title = $2" js_set_title
  :: Document -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.dir" js_get_dir
  :: Document -> (IO DOMString)
foreign import javascript unsafe "$1.dir = $2" js_set_dir
  :: Document -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.body" js_get_body
  :: Document -> (IO (Nullable HTMLElementClass))
foreign import javascript unsafe "$1.body = $2" js_set_body
  :: Document -> (Nullable HTMLElementClass -> (IO ()))
foreign import javascript unsafe "$1.head" js_get_head
  :: Document -> (IO (Nullable HTMLHeadElementClass))
foreign import javascript unsafe "$1.images" js_get_images
  :: Document -> (IO HTMLCollection)
foreign import javascript unsafe "$1.embeds" js_get_embeds
  :: Document -> (IO HTMLCollection)
foreign import javascript unsafe "$1.plugins" js_get_plugins
  :: Document -> (IO HTMLCollection)
foreign import javascript unsafe "$1.links" js_get_links
  :: Document -> (IO HTMLCollection)
foreign import javascript unsafe "$1.forms" js_get_forms
  :: Document -> (IO HTMLCollection)
foreign import javascript unsafe "$1.scripts" js_get_scripts
  :: Document -> (IO HTMLCollection)
foreign import javascript unsafe "$1.defaultView" js_get_defaultView
  :: Document -> (IO (Nullable WindowProxyClass))
foreign import javascript unsafe "$1.onreadystatechange" js_get_onreadystatechange
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onreadystatechange = $2" js_set_onreadystatechange
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onbeforescriptexecute" js_get_onbeforescriptexecute
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onbeforescriptexecute = $2" js_set_onbeforescriptexecute
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onafterscriptexecute" js_get_onafterscriptexecute
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onafterscriptexecute = $2" js_set_onafterscriptexecute
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onselectionchange" js_get_onselectionchange
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onselectionchange = $2" js_set_onselectionchange
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.currentScript" js_get_currentScript
  :: Document -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.documentURIObject" js_get_documentURIObject
  :: Document -> (IO (Nullable URIClass))
foreign import javascript unsafe "$1.referrerPolicy" js_get_referrerPolicy
  :: Document -> (IO Word32)
foreign import javascript unsafe "$1.anchors" js_get_anchors
  :: Document -> (IO HTMLCollection)
foreign import javascript unsafe "$1.applets" js_get_applets
  :: Document -> (IO HTMLCollection)
foreign import javascript unsafe "$1.fullscreen" js_get_fullscreen
  :: Document -> (IO Bool)
foreign import javascript unsafe "$1.fullscreenEnabled" js_get_fullscreenEnabled
  :: Document -> (IO Bool)
foreign import javascript unsafe "$1.onfullscreenchange" js_get_onfullscreenchange
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onfullscreenchange = $2" js_set_onfullscreenchange
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onfullscreenerror" js_get_onfullscreenerror
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onfullscreenerror = $2" js_set_onfullscreenerror
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerlockchange" js_get_onpointerlockchange
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerlockchange = $2" js_set_onpointerlockchange
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerlockerror" js_get_onpointerlockerror
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerlockerror = $2" js_set_onpointerlockerror
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.hidden" js_get_hidden
  :: Document -> (IO Bool)
foreign import javascript unsafe "$1.visibilityState" js_get_visibilityState
  :: Document -> (IO VisibilityState)
foreign import javascript unsafe "$1.onvisibilitychange" js_get_onvisibilitychange
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onvisibilitychange = $2" js_set_onvisibilitychange
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.selectedStyleSheetSet" js_get_selectedStyleSheetSet
  :: Document -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.selectedStyleSheetSet = $2" js_set_selectedStyleSheetSet
  :: Document -> (Nullable DOMStringClass -> (IO ()))
foreign import javascript unsafe "$1.lastStyleSheetSet" js_get_lastStyleSheetSet
  :: Document -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.preferredStyleSheetSet" js_get_preferredStyleSheetSet
  :: Document -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.styleSheetSets" js_get_styleSheetSets
  :: Document -> (IO DOMStringList)
foreign import javascript unsafe "$1.scrollingElement" js_get_scrollingElement
  :: Document -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.timeline" js_get_timeline
  :: Document -> (IO DocumentTimeline)
foreign import javascript unsafe "$1.rootElement" js_get_rootElement
  :: Document -> (IO (Nullable SVGSVGElementClass))
foreign import javascript unsafe "$1.isSrcdocDocument" js_get_isSrcdocDocument
  :: Document -> (IO Bool)
foreign import javascript unsafe "$1.sandboxFlagsAsString" js_get_sandboxFlagsAsString
  :: Document -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.userHasInteracted" js_get_userHasInteracted
  :: Document -> (IO Bool)
foreign import javascript unsafe "$1.documentFlashClassification" js_get_documentFlashClassification
  :: Document -> (IO FlashClassification)
foreign import javascript unsafe "$1.onabort" js_get_onabort
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onabort = $2" js_set_onabort
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onblur" js_get_onblur
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onblur = $2" js_set_onblur
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onfocus" js_get_onfocus
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onfocus = $2" js_set_onfocus
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onauxclick" js_get_onauxclick
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onauxclick = $2" js_set_onauxclick
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncanplay" js_get_oncanplay
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.oncanplay = $2" js_set_oncanplay
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncanplaythrough" js_get_oncanplaythrough
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.oncanplaythrough = $2" js_set_oncanplaythrough
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onchange" js_get_onchange
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onchange = $2" js_set_onchange
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onclick" js_get_onclick
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onclick = $2" js_set_onclick
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onclose" js_get_onclose
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onclose = $2" js_set_onclose
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncontextmenu" js_get_oncontextmenu
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.oncontextmenu = $2" js_set_oncontextmenu
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondblclick" js_get_ondblclick
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ondblclick = $2" js_set_ondblclick
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondrag" js_get_ondrag
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ondrag = $2" js_set_ondrag
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragend" js_get_ondragend
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragend = $2" js_set_ondragend
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragenter" js_get_ondragenter
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragenter = $2" js_set_ondragenter
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragexit" js_get_ondragexit
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragexit = $2" js_set_ondragexit
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragleave" js_get_ondragleave
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragleave = $2" js_set_ondragleave
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragover" js_get_ondragover
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragover = $2" js_set_ondragover
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragstart" js_get_ondragstart
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragstart = $2" js_set_ondragstart
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondrop" js_get_ondrop
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ondrop = $2" js_set_ondrop
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondurationchange" js_get_ondurationchange
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ondurationchange = $2" js_set_ondurationchange
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onemptied" js_get_onemptied
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onemptied = $2" js_set_onemptied
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onended" js_get_onended
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onended = $2" js_set_onended
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oninput" js_get_oninput
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.oninput = $2" js_set_oninput
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oninvalid" js_get_oninvalid
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.oninvalid = $2" js_set_oninvalid
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onkeydown" js_get_onkeydown
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onkeydown = $2" js_set_onkeydown
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onkeypress" js_get_onkeypress
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onkeypress = $2" js_set_onkeypress
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onkeyup" js_get_onkeyup
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onkeyup = $2" js_set_onkeyup
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onload" js_get_onload
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onload = $2" js_set_onload
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadeddata" js_get_onloadeddata
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadeddata = $2" js_set_onloadeddata
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadedmetadata" js_get_onloadedmetadata
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadedmetadata = $2" js_set_onloadedmetadata
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadend" js_get_onloadend
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadend = $2" js_set_onloadend
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadstart" js_get_onloadstart
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadstart = $2" js_set_onloadstart
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmousedown" js_get_onmousedown
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onmousedown = $2" js_set_onmousedown
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseenter" js_get_onmouseenter
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseenter = $2" js_set_onmouseenter
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseleave" js_get_onmouseleave
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseleave = $2" js_set_onmouseleave
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmousemove" js_get_onmousemove
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onmousemove = $2" js_set_onmousemove
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseout" js_get_onmouseout
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseout = $2" js_set_onmouseout
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseover" js_get_onmouseover
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseover = $2" js_set_onmouseover
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseup" js_get_onmouseup
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseup = $2" js_set_onmouseup
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwheel" js_get_onwheel
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onwheel = $2" js_set_onwheel
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpause" js_get_onpause
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onpause = $2" js_set_onpause
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onplay" js_get_onplay
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onplay = $2" js_set_onplay
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onplaying" js_get_onplaying
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onplaying = $2" js_set_onplaying
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onprogress" js_get_onprogress
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onprogress = $2" js_set_onprogress
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onratechange" js_get_onratechange
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onratechange = $2" js_set_onratechange
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onreset" js_get_onreset
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onreset = $2" js_set_onreset
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onresize" js_get_onresize
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onresize = $2" js_set_onresize
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onscroll" js_get_onscroll
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onscroll = $2" js_set_onscroll
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onseeked" js_get_onseeked
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onseeked = $2" js_set_onseeked
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onseeking" js_get_onseeking
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onseeking = $2" js_set_onseeking
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onselect" js_get_onselect
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onselect = $2" js_set_onselect
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onshow" js_get_onshow
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onshow = $2" js_set_onshow
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onstalled" js_get_onstalled
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onstalled = $2" js_set_onstalled
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onsubmit" js_get_onsubmit
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onsubmit = $2" js_set_onsubmit
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onsuspend" js_get_onsuspend
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onsuspend = $2" js_set_onsuspend
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontimeupdate" js_get_ontimeupdate
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ontimeupdate = $2" js_set_ontimeupdate
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onvolumechange" js_get_onvolumechange
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onvolumechange = $2" js_set_onvolumechange
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwaiting" js_get_onwaiting
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onwaiting = $2" js_set_onwaiting
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onselectstart" js_get_onselectstart
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onselectstart = $2" js_set_onselectstart
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontoggle" js_get_ontoggle
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ontoggle = $2" js_set_ontoggle
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointercancel" js_get_onpointercancel
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointercancel = $2" js_set_onpointercancel
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerdown" js_get_onpointerdown
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerdown = $2" js_set_onpointerdown
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerup" js_get_onpointerup
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerup = $2" js_set_onpointerup
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointermove" js_get_onpointermove
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointermove = $2" js_set_onpointermove
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerout" js_get_onpointerout
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerout = $2" js_set_onpointerout
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerover" js_get_onpointerover
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerover = $2" js_set_onpointerover
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerenter" js_get_onpointerenter
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerenter = $2" js_set_onpointerenter
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerleave" js_get_onpointerleave
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerleave = $2" js_set_onpointerleave
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ongotpointercapture" js_get_ongotpointercapture
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ongotpointercapture = $2" js_set_ongotpointercapture
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onlostpointercapture" js_get_onlostpointercapture
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onlostpointercapture = $2" js_set_onlostpointercapture
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onanimationcancel" js_get_onanimationcancel
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onanimationcancel = $2" js_set_onanimationcancel
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onanimationend" js_get_onanimationend
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onanimationend = $2" js_set_onanimationend
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onanimationiteration" js_get_onanimationiteration
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onanimationiteration = $2" js_set_onanimationiteration
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onanimationstart" js_get_onanimationstart
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onanimationstart = $2" js_set_onanimationstart
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontransitioncancel" js_get_ontransitioncancel
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ontransitioncancel = $2" js_set_ontransitioncancel
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontransitionend" js_get_ontransitionend
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ontransitionend = $2" js_set_ontransitionend
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontransitionrun" js_get_ontransitionrun
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ontransitionrun = $2" js_set_ontransitionrun
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontransitionstart" js_get_ontransitionstart
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ontransitionstart = $2" js_set_ontransitionstart
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwebkitanimationend" js_get_onwebkitanimationend
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onwebkitanimationend = $2" js_set_onwebkitanimationend
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwebkitanimationiteration" js_get_onwebkitanimationiteration
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onwebkitanimationiteration = $2" js_set_onwebkitanimationiteration
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwebkitanimationstart" js_get_onwebkitanimationstart
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onwebkitanimationstart = $2" js_set_onwebkitanimationstart
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwebkittransitionend" js_get_onwebkittransitionend
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onwebkittransitionend = $2" js_set_onwebkittransitionend
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncopy" js_get_oncopy
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.oncopy = $2" js_set_oncopy
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncut" js_get_oncut
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.oncut = $2" js_set_oncut
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpaste" js_get_onpaste
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onpaste = $2" js_set_onpaste
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontouchstart" js_get_ontouchstart
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ontouchstart = $2" js_set_ontouchstart
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontouchend" js_get_ontouchend
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ontouchend = $2" js_set_ontouchend
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontouchmove" js_get_ontouchmove
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ontouchmove = $2" js_set_ontouchmove
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontouchcancel" js_get_ontouchcancel
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.ontouchcancel = $2" js_set_ontouchcancel
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.children" js_get_children
  :: Document -> (IO HTMLCollection)
foreign import javascript unsafe "$1.firstElementChild" js_get_firstElementChild
  :: Document -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.lastElementChild" js_get_lastElementChild
  :: Document -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.childElementCount" js_get_childElementCount
  :: Document -> (IO Word32)
foreign import javascript unsafe "$1.onerror" js_get_onerror
  :: Document -> (IO EventHandler)
foreign import javascript unsafe "$1.onerror = $2" js_set_onerror
  :: Document -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.fonts" js_get_fonts
  :: Document -> (IO FontFaceSet)
foreign import javascript unsafe "$1.activeElement" js_get_activeElement
  :: Document -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.styleSheets" js_get_styleSheets
  :: Document -> (IO StyleSheetList)
foreign import javascript unsafe "$1.pointerLockElement" js_get_pointerLockElement
  :: Document -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.fullscreenElement" js_get_fullscreenElement
  :: Document -> (IO (Nullable ElementClass))
