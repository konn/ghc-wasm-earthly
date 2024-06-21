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
module GHC.Wasm.Web.Generated.DocumentType (
        DocumentType, DocumentTypeClass,
        js_fun_before_Union_Node_DOMString_EndUnion_undefined,
        js_fun_after_Union_Node_DOMString_EndUnion_undefined,
        js_fun_replaceWith_Union_Node_DOMString_EndUnion_undefined,
        js_fun_remove__undefined, js_get_name, js_get_publicId,
        js_get_systemId
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DocumentType.Core
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.before(... $2)" js_fun_before_Union_Node_DOMString_EndUnion_undefined
  :: DocumentType
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.after(... $2)" js_fun_after_Union_Node_DOMString_EndUnion_undefined
  :: DocumentType
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.replaceWith(... $2)" js_fun_replaceWith_Union_Node_DOMString_EndUnion_undefined
  :: DocumentType
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.remove()" js_fun_remove__undefined
  :: DocumentType -> (IO ())
foreign import javascript unsafe "$1.name" js_get_name
  :: DocumentType -> (IO DOMString)
foreign import javascript unsafe "$1.publicId" js_get_publicId
  :: DocumentType -> (IO DOMString)
foreign import javascript unsafe "$1.systemId" js_get_systemId
  :: DocumentType -> (IO DOMString)
