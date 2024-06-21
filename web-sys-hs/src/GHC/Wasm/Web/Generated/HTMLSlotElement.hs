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
module GHC.Wasm.Web.Generated.HTMLSlotElement (
        HTMLSlotElement, HTMLSlotElementClass,
        js_fun_assignedNodes_nullable_AssignedNodesOptions_sequence_Node,
        js_get_name, js_set_name
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AssignedNodesOptions.Core
import GHC.Wasm.Web.Generated.HTMLElement.Core
import GHC.Wasm.Web.Generated.HTMLSlotElement.Core
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.assignedNodes($2)" js_fun_assignedNodes_nullable_AssignedNodesOptions_sequence_Node
  :: HTMLSlotElement
     -> (Nullable AssignedNodesOptionsClass
         -> (IO (Sequence NodeClass)))
foreign import javascript unsafe "$1.name" js_get_name
  :: HTMLSlotElement -> (IO DOMString)
foreign import javascript unsafe "$1.name = $2" js_set_name
  :: HTMLSlotElement -> (DOMString -> (IO ()))
