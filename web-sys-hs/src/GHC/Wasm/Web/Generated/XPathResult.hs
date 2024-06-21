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
module GHC.Wasm.Web.Generated.XPathResult (
        XPathResult, XPathResultClass, js_const_XPathResult_ANY_TYPE,
        js_const_XPathResult_NUMBER_TYPE, js_const_XPathResult_STRING_TYPE,
        js_const_XPathResult_BOOLEAN_TYPE,
        js_const_XPathResult_UNORDERED_NODE_ITERATOR_TYPE,
        js_const_XPathResult_ORDERED_NODE_ITERATOR_TYPE,
        js_const_XPathResult_UNORDERED_NODE_SNAPSHOT_TYPE,
        js_const_XPathResult_ORDERED_NODE_SNAPSHOT_TYPE,
        js_const_XPathResult_ANY_UNORDERED_NODE_TYPE,
        js_const_XPathResult_FIRST_ORDERED_NODE_TYPE,
        js_fun_iterateNext__nullable_Node,
        js_fun_snapshotItem_long_nullable_Node, js_get_resultType,
        js_get_numberValue, js_get_stringValue, js_get_booleanValue,
        js_get_singleNodeValue, js_get_invalidIteratorState,
        js_get_snapshotLength
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Generated.XPathResult.Core
import GHC.Wasm.Web.Types
js_const_XPathResult_ANY_TYPE :: Word16
js_const_XPathResult_ANY_TYPE = 0
js_const_XPathResult_NUMBER_TYPE :: Word16
js_const_XPathResult_NUMBER_TYPE = 1
js_const_XPathResult_STRING_TYPE :: Word16
js_const_XPathResult_STRING_TYPE = 2
js_const_XPathResult_BOOLEAN_TYPE :: Word16
js_const_XPathResult_BOOLEAN_TYPE = 3
js_const_XPathResult_UNORDERED_NODE_ITERATOR_TYPE :: Word16
js_const_XPathResult_UNORDERED_NODE_ITERATOR_TYPE = 4
js_const_XPathResult_ORDERED_NODE_ITERATOR_TYPE :: Word16
js_const_XPathResult_ORDERED_NODE_ITERATOR_TYPE = 5
js_const_XPathResult_UNORDERED_NODE_SNAPSHOT_TYPE :: Word16
js_const_XPathResult_UNORDERED_NODE_SNAPSHOT_TYPE = 6
js_const_XPathResult_ORDERED_NODE_SNAPSHOT_TYPE :: Word16
js_const_XPathResult_ORDERED_NODE_SNAPSHOT_TYPE = 7
js_const_XPathResult_ANY_UNORDERED_NODE_TYPE :: Word16
js_const_XPathResult_ANY_UNORDERED_NODE_TYPE = 8
js_const_XPathResult_FIRST_ORDERED_NODE_TYPE :: Word16
js_const_XPathResult_FIRST_ORDERED_NODE_TYPE = 9
foreign import javascript unsafe "$1.iterateNext()" js_fun_iterateNext__nullable_Node
  :: XPathResult -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.snapshotItem($2)" js_fun_snapshotItem_long_nullable_Node
  :: XPathResult -> (Word32 -> (IO (Nullable NodeClass)))
foreign import javascript unsafe "$1.resultType" js_get_resultType
  :: XPathResult -> (IO Word16)
foreign import javascript unsafe "$1.numberValue" js_get_numberValue
  :: XPathResult -> (IO Double)
foreign import javascript unsafe "$1.stringValue" js_get_stringValue
  :: XPathResult -> (IO DOMString)
foreign import javascript unsafe "$1.booleanValue" js_get_booleanValue
  :: XPathResult -> (IO Bool)
foreign import javascript unsafe "$1.singleNodeValue" js_get_singleNodeValue
  :: XPathResult -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.invalidIteratorState" js_get_invalidIteratorState
  :: XPathResult -> (IO Bool)
foreign import javascript unsafe "$1.snapshotLength" js_get_snapshotLength
  :: XPathResult -> (IO Word32)
