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
module GHC.Wasm.Web.Generated.TreeWalker (
        TreeWalker, TreeWalkerClass, js_fun_parentNode__nullable_Node,
        js_fun_firstChild__nullable_Node, js_fun_lastChild__nullable_Node,
        js_fun_previousSibling__nullable_Node,
        js_fun_nextSibling__nullable_Node,
        js_fun_previousNode__nullable_Node, js_fun_nextNode__nullable_Node,
        js_get_root, js_get_whatToShow, js_get_filter, js_get_currentNode,
        js_set_currentNode
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Generated.NodeFilter.Core
import GHC.Wasm.Web.Generated.TreeWalker.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.parentNode()" js_fun_parentNode__nullable_Node
  :: TreeWalker -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.firstChild()" js_fun_firstChild__nullable_Node
  :: TreeWalker -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.lastChild()" js_fun_lastChild__nullable_Node
  :: TreeWalker -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.previousSibling()" js_fun_previousSibling__nullable_Node
  :: TreeWalker -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.nextSibling()" js_fun_nextSibling__nullable_Node
  :: TreeWalker -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.previousNode()" js_fun_previousNode__nullable_Node
  :: TreeWalker -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.nextNode()" js_fun_nextNode__nullable_Node
  :: TreeWalker -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.root" js_get_root
  :: TreeWalker -> (IO Node)
foreign import javascript unsafe "$1.whatToShow" js_get_whatToShow
  :: TreeWalker -> (IO Word32)
foreign import javascript unsafe "$1.filter" js_get_filter
  :: TreeWalker -> (IO (Nullable NodeFilterClass))
foreign import javascript unsafe "$1.currentNode" js_get_currentNode
  :: TreeWalker -> (IO Node)
foreign import javascript unsafe "$1.currentNode = $2" js_set_currentNode
  :: TreeWalker -> (Node -> (IO ()))
