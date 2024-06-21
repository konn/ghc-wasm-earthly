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
module GHC.Wasm.Web.Generated.NodeIterator (
        NodeIterator, NodeIteratorClass, js_fun_nextNode__nullable_Node,
        js_fun_previousNode__nullable_Node, js_fun_detach__undefined,
        js_get_root, js_get_referenceNode,
        js_get_pointerBeforeReferenceNode, js_get_whatToShow, js_get_filter
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Generated.NodeFilter.Core
import GHC.Wasm.Web.Generated.NodeIterator.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.nextNode()" js_fun_nextNode__nullable_Node
  :: NodeIterator -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.previousNode()" js_fun_previousNode__nullable_Node
  :: NodeIterator -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.detach()" js_fun_detach__undefined
  :: NodeIterator -> (IO ())
foreign import javascript unsafe "$1.root" js_get_root
  :: NodeIterator -> (IO Node)
foreign import javascript unsafe "$1.referenceNode" js_get_referenceNode
  :: NodeIterator -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.pointerBeforeReferenceNode" js_get_pointerBeforeReferenceNode
  :: NodeIterator -> (IO Bool)
foreign import javascript unsafe "$1.whatToShow" js_get_whatToShow
  :: NodeIterator -> (IO Word32)
foreign import javascript unsafe "$1.filter" js_get_filter
  :: NodeIterator -> (IO (Nullable NodeFilterClass))
