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
module GHC.Wasm.Web.Generated.Node (
        Node, NodeClass, js_const_Node_ELEMENT_NODE,
        js_const_Node_ATTRIBUTE_NODE, js_const_Node_TEXT_NODE,
        js_const_Node_CDATA_SECTION_NODE,
        js_const_Node_ENTITY_REFERENCE_NODE, js_const_Node_ENTITY_NODE,
        js_const_Node_PROCESSING_INSTRUCTION_NODE,
        js_const_Node_COMMENT_NODE, js_const_Node_DOCUMENT_NODE,
        js_const_Node_DOCUMENT_TYPE_NODE,
        js_const_Node_DOCUMENT_FRAGMENT_NODE, js_const_Node_NOTATION_NODE,
        js_const_Node_DOCUMENT_POSITION_DISCONNECTED,
        js_const_Node_DOCUMENT_POSITION_PRECEDING,
        js_const_Node_DOCUMENT_POSITION_FOLLOWING,
        js_const_Node_DOCUMENT_POSITION_CONTAINS,
        js_const_Node_DOCUMENT_POSITION_CONTAINED_BY,
        js_const_Node_DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC,
        js_fun_getRootNode_nullable_GetRootNodeOptions_Node,
        js_fun_hasChildNodes__boolean,
        js_fun_insertBefore_Node_nullable_Node_Node,
        js_fun_appendChild_Node_Node, js_fun_replaceChild_Node_Node_Node,
        js_fun_removeChild_Node_Node, js_fun_normalize__undefined,
        js_fun_cloneNode_nullable_boolean_Node,
        js_fun_isSameNode_nullable_Node_boolean,
        js_fun_isEqualNode_nullable_Node_boolean,
        js_fun_compareDocumentPosition_Node_short,
        js_fun_contains_nullable_Node_boolean,
        js_fun_lookupPrefix_nullable_DOMString_nullable_DOMString,
        js_fun_lookupNamespaceURI_nullable_DOMString_nullable_DOMString,
        js_fun_isDefaultNamespace_nullable_DOMString_boolean,
        js_get_nodeType, js_get_nodeName, js_get_baseURI,
        js_get_isConnected, js_get_ownerDocument, js_get_parentNode,
        js_get_parentElement, js_get_childNodes, js_get_firstChild,
        js_get_lastChild, js_get_previousSibling, js_get_nextSibling,
        js_get_nodeValue, js_set_nodeValue, js_get_textContent,
        js_set_textContent
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Document.Core
import GHC.Wasm.Web.Generated.Element.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.GetRootNodeOptions.Core
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Generated.NodeList.Core
import GHC.Wasm.Web.Types
js_const_Node_ELEMENT_NODE :: Word16
js_const_Node_ELEMENT_NODE = 1
js_const_Node_ATTRIBUTE_NODE :: Word16
js_const_Node_ATTRIBUTE_NODE = 2
js_const_Node_TEXT_NODE :: Word16
js_const_Node_TEXT_NODE = 3
js_const_Node_CDATA_SECTION_NODE :: Word16
js_const_Node_CDATA_SECTION_NODE = 4
js_const_Node_ENTITY_REFERENCE_NODE :: Word16
js_const_Node_ENTITY_REFERENCE_NODE = 5
js_const_Node_ENTITY_NODE :: Word16
js_const_Node_ENTITY_NODE = 6
js_const_Node_PROCESSING_INSTRUCTION_NODE :: Word16
js_const_Node_PROCESSING_INSTRUCTION_NODE = 7
js_const_Node_COMMENT_NODE :: Word16
js_const_Node_COMMENT_NODE = 8
js_const_Node_DOCUMENT_NODE :: Word16
js_const_Node_DOCUMENT_NODE = 9
js_const_Node_DOCUMENT_TYPE_NODE :: Word16
js_const_Node_DOCUMENT_TYPE_NODE = 10
js_const_Node_DOCUMENT_FRAGMENT_NODE :: Word16
js_const_Node_DOCUMENT_FRAGMENT_NODE = 11
js_const_Node_NOTATION_NODE :: Word16
js_const_Node_NOTATION_NODE = 12
js_const_Node_DOCUMENT_POSITION_DISCONNECTED :: Word16
js_const_Node_DOCUMENT_POSITION_DISCONNECTED = 1
js_const_Node_DOCUMENT_POSITION_PRECEDING :: Word16
js_const_Node_DOCUMENT_POSITION_PRECEDING = 2
js_const_Node_DOCUMENT_POSITION_FOLLOWING :: Word16
js_const_Node_DOCUMENT_POSITION_FOLLOWING = 4
js_const_Node_DOCUMENT_POSITION_CONTAINS :: Word16
js_const_Node_DOCUMENT_POSITION_CONTAINS = 8
js_const_Node_DOCUMENT_POSITION_CONTAINED_BY :: Word16
js_const_Node_DOCUMENT_POSITION_CONTAINED_BY = 16
js_const_Node_DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC :: Word16
js_const_Node_DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC = 32
foreign import javascript unsafe "$1.getRootNode($2)" js_fun_getRootNode_nullable_GetRootNodeOptions_Node
  :: Node -> (Nullable GetRootNodeOptionsClass -> (IO Node))
foreign import javascript unsafe "$1.hasChildNodes()" js_fun_hasChildNodes__boolean
  :: Node -> (IO Bool)
foreign import javascript unsafe "$1.insertBefore($2,$3)" js_fun_insertBefore_Node_nullable_Node_Node
  :: Node -> (Node -> (Nullable NodeClass -> (IO Node)))
foreign import javascript unsafe "$1.appendChild($2)" js_fun_appendChild_Node_Node
  :: Node -> (Node -> (IO Node))
foreign import javascript unsafe "$1.replaceChild($2,$3)" js_fun_replaceChild_Node_Node_Node
  :: Node -> (Node -> (Node -> (IO Node)))
foreign import javascript unsafe "$1.removeChild($2)" js_fun_removeChild_Node_Node
  :: Node -> (Node -> (IO Node))
foreign import javascript unsafe "$1.normalize()" js_fun_normalize__undefined
  :: Node -> (IO ())
foreign import javascript unsafe "$1.cloneNode($2)" js_fun_cloneNode_nullable_boolean_Node
  :: Node -> (Nullable (JSPrimClass Bool) -> (IO Node))
foreign import javascript unsafe "$1.isSameNode($2)" js_fun_isSameNode_nullable_Node_boolean
  :: Node -> (Nullable NodeClass -> (IO Bool))
foreign import javascript unsafe "$1.isEqualNode($2)" js_fun_isEqualNode_nullable_Node_boolean
  :: Node -> (Nullable NodeClass -> (IO Bool))
foreign import javascript unsafe "$1.compareDocumentPosition($2)" js_fun_compareDocumentPosition_Node_short
  :: Node -> (Node -> (IO Word16))
foreign import javascript unsafe "$1.contains($2)" js_fun_contains_nullable_Node_boolean
  :: Node -> (Nullable NodeClass -> (IO Bool))
foreign import javascript unsafe "$1.lookupPrefix($2)" js_fun_lookupPrefix_nullable_DOMString_nullable_DOMString
  :: Node
     -> (Nullable DOMStringClass -> (IO (Nullable DOMStringClass)))
foreign import javascript unsafe "$1.lookupNamespaceURI($2)" js_fun_lookupNamespaceURI_nullable_DOMString_nullable_DOMString
  :: Node
     -> (Nullable DOMStringClass -> (IO (Nullable DOMStringClass)))
foreign import javascript unsafe "$1.isDefaultNamespace($2)" js_fun_isDefaultNamespace_nullable_DOMString_boolean
  :: Node -> (Nullable DOMStringClass -> (IO Bool))
foreign import javascript unsafe "$1.nodeType" js_get_nodeType
  :: Node -> (IO Word16)
foreign import javascript unsafe "$1.nodeName" js_get_nodeName
  :: Node -> (IO DOMString)
foreign import javascript unsafe "$1.baseURI" js_get_baseURI
  :: Node -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.isConnected" js_get_isConnected
  :: Node -> (IO Bool)
foreign import javascript unsafe "$1.ownerDocument" js_get_ownerDocument
  :: Node -> (IO (Nullable DocumentClass))
foreign import javascript unsafe "$1.parentNode" js_get_parentNode
  :: Node -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.parentElement" js_get_parentElement
  :: Node -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.childNodes" js_get_childNodes
  :: Node -> (IO NodeList)
foreign import javascript unsafe "$1.firstChild" js_get_firstChild
  :: Node -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.lastChild" js_get_lastChild
  :: Node -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.previousSibling" js_get_previousSibling
  :: Node -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.nextSibling" js_get_nextSibling
  :: Node -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.nodeValue" js_get_nodeValue
  :: Node -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.nodeValue = $2" js_set_nodeValue
  :: Node -> (Nullable DOMStringClass -> (IO ()))
foreign import javascript unsafe "$1.textContent" js_get_textContent
  :: Node -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.textContent = $2" js_set_textContent
  :: Node -> (Nullable DOMStringClass -> (IO ()))
