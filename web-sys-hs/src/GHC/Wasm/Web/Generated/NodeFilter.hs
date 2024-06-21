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
module GHC.Wasm.Web.Generated.NodeFilter (
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
import GHC.Wasm.Web.Generated.NodeFilter.Core
import GHC.Wasm.Web.Types
