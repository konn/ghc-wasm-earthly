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
module GHC.Wasm.Web.Generated.NodeFilter.Core (
        NodeFilterClass, NodeFilter, js_mk_callback_NodeFilter,
        js_const_NodeFilter_FILTER_ACCEPT,
        js_const_NodeFilter_FILTER_REJECT, js_const_NodeFilter_FILTER_SKIP,
        js_const_NodeFilter_SHOW_ALL, js_const_NodeFilter_SHOW_ELEMENT,
        js_const_NodeFilter_SHOW_ATTRIBUTE, js_const_NodeFilter_SHOW_TEXT,
        js_const_NodeFilter_SHOW_CDATA_SECTION,
        js_const_NodeFilter_SHOW_ENTITY_REFERENCE,
        js_const_NodeFilter_SHOW_ENTITY,
        js_const_NodeFilter_SHOW_PROCESSING_INSTRUCTION,
        js_const_NodeFilter_SHOW_COMMENT,
        js_const_NodeFilter_SHOW_DOCUMENT,
        js_const_NodeFilter_SHOW_DOCUMENT_TYPE,
        js_const_NodeFilter_SHOW_DOCUMENT_FRAGMENT,
        js_const_NodeFilter_SHOW_NOTATION
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Types
type data NodeFilterClass :: Prototype
type instance SuperclassOf NodeFilterClass = 'Nothing
type NodeFilter = JSObject NodeFilterClass
foreign import javascript unsafe "wrapper" js_mk_callback_NodeFilter
  :: (Node -> (IO Word16)) -> NodeFilter
js_const_NodeFilter_FILTER_ACCEPT :: Word16
js_const_NodeFilter_FILTER_ACCEPT = 1
js_const_NodeFilter_FILTER_REJECT :: Word16
js_const_NodeFilter_FILTER_REJECT = 2
js_const_NodeFilter_FILTER_SKIP :: Word16
js_const_NodeFilter_FILTER_SKIP = 3
js_const_NodeFilter_SHOW_ALL :: Word32
js_const_NodeFilter_SHOW_ALL = 4294967295
js_const_NodeFilter_SHOW_ELEMENT :: Word32
js_const_NodeFilter_SHOW_ELEMENT = 1
js_const_NodeFilter_SHOW_ATTRIBUTE :: Word32
js_const_NodeFilter_SHOW_ATTRIBUTE = 2
js_const_NodeFilter_SHOW_TEXT :: Word32
js_const_NodeFilter_SHOW_TEXT = 4
js_const_NodeFilter_SHOW_CDATA_SECTION :: Word32
js_const_NodeFilter_SHOW_CDATA_SECTION = 8
js_const_NodeFilter_SHOW_ENTITY_REFERENCE :: Word32
js_const_NodeFilter_SHOW_ENTITY_REFERENCE = 16
js_const_NodeFilter_SHOW_ENTITY :: Word32
js_const_NodeFilter_SHOW_ENTITY = 32
js_const_NodeFilter_SHOW_PROCESSING_INSTRUCTION :: Word32
js_const_NodeFilter_SHOW_PROCESSING_INSTRUCTION = 64
js_const_NodeFilter_SHOW_COMMENT :: Word32
js_const_NodeFilter_SHOW_COMMENT = 128
js_const_NodeFilter_SHOW_DOCUMENT :: Word32
js_const_NodeFilter_SHOW_DOCUMENT = 256
js_const_NodeFilter_SHOW_DOCUMENT_TYPE :: Word32
js_const_NodeFilter_SHOW_DOCUMENT_TYPE = 512
js_const_NodeFilter_SHOW_DOCUMENT_FRAGMENT :: Word32
js_const_NodeFilter_SHOW_DOCUMENT_FRAGMENT = 1024
js_const_NodeFilter_SHOW_NOTATION :: Word32
js_const_NodeFilter_SHOW_NOTATION = 2048
