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
module GHC.Wasm.Web.Generated.CustomElementRegistry (
        CustomElementRegistry, CustomElementRegistryClass,
        js_fun_define_DOMString_Function_nullable_ElementDefinitionOptions_undefined,
        js_fun_setElementCreationCallback_DOMString_CustomElementCreationCallback_undefined,
        js_fun_get_DOMString_any,
        js_fun_whenDefined_DOMString_Promise_undefined,
        js_fun_upgrade_Node_undefined
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.CustomElementCreationCallback.Core
import GHC.Wasm.Web.Generated.CustomElementRegistry.Core
import GHC.Wasm.Web.Generated.ElementDefinitionOptions.Core
import GHC.Wasm.Web.Generated.Function.Core
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.define($2,$3,$4)" js_fun_define_DOMString_Function_nullable_ElementDefinitionOptions_undefined
  :: CustomElementRegistry
     -> (DOMString
         -> (Function
             -> (Nullable ElementDefinitionOptionsClass -> (IO ()))))
foreign import javascript unsafe "$1.setElementCreationCallback($2,$3)" js_fun_setElementCreationCallback_DOMString_CustomElementCreationCallback_undefined
  :: CustomElementRegistry
     -> (DOMString -> (CustomElementCreationCallback -> (IO ())))
foreign import javascript unsafe "$1.get($2)" js_fun_get_DOMString_any
  :: CustomElementRegistry -> (DOMString -> (IO JSAny))
foreign import javascript safe "$1.whenDefined($2)" js_fun_whenDefined_DOMString_Promise_undefined
  :: CustomElementRegistry
     -> (DOMString -> (IO (Promise UndefinedClass)))
foreign import javascript unsafe "$1.upgrade($2)" js_fun_upgrade_Node_undefined
  :: CustomElementRegistry -> (Node -> (IO ()))
