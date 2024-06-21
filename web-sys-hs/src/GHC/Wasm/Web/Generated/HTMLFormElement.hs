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
module GHC.Wasm.Web.Generated.HTMLFormElement (
        HTMLFormElement, HTMLFormElementClass, js_fun_submit__undefined,
        js_fun_requestSubmit_nullable_nullable_HTMLElement_undefined,
        js_fun_reset__undefined, js_fun_checkValidity__boolean,
        js_fun_reportValidity__boolean, js_get_acceptCharset,
        js_set_acceptCharset, js_get_action, js_set_action,
        js_get_autocomplete, js_set_autocomplete, js_get_enctype,
        js_set_enctype, js_get_encoding, js_set_encoding, js_get_method,
        js_set_method, js_get_name, js_set_name, js_get_noValidate,
        js_set_noValidate, js_get_target, js_set_target, js_get_elements,
        js_get_length
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Element.Core
import GHC.Wasm.Web.Generated.HTMLCollection.Core
import GHC.Wasm.Web.Generated.HTMLElement.Core
import GHC.Wasm.Web.Generated.HTMLFormElement.Core
import GHC.Wasm.Web.Generated.NsISupports.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.submit()" js_fun_submit__undefined
  :: HTMLFormElement -> (IO ())
foreign import javascript unsafe "$1.requestSubmit($2)" js_fun_requestSubmit_nullable_nullable_HTMLElement_undefined
  :: HTMLFormElement
     -> (Nullable (NullableClass HTMLElementClass) -> (IO ()))
foreign import javascript unsafe "$1.reset()" js_fun_reset__undefined
  :: HTMLFormElement -> (IO ())
foreign import javascript unsafe "$1.checkValidity()" js_fun_checkValidity__boolean
  :: HTMLFormElement -> (IO Bool)
foreign import javascript unsafe "$1.reportValidity()" js_fun_reportValidity__boolean
  :: HTMLFormElement -> (IO Bool)
foreign import javascript unsafe "$1.acceptCharset" js_get_acceptCharset
  :: HTMLFormElement -> (IO DOMString)
foreign import javascript unsafe "$1.acceptCharset = $2" js_set_acceptCharset
  :: HTMLFormElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.action" js_get_action
  :: HTMLFormElement -> (IO DOMString)
foreign import javascript unsafe "$1.action = $2" js_set_action
  :: HTMLFormElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.autocomplete" js_get_autocomplete
  :: HTMLFormElement -> (IO DOMString)
foreign import javascript unsafe "$1.autocomplete = $2" js_set_autocomplete
  :: HTMLFormElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.enctype" js_get_enctype
  :: HTMLFormElement -> (IO DOMString)
foreign import javascript unsafe "$1.enctype = $2" js_set_enctype
  :: HTMLFormElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.encoding" js_get_encoding
  :: HTMLFormElement -> (IO DOMString)
foreign import javascript unsafe "$1.encoding = $2" js_set_encoding
  :: HTMLFormElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.method" js_get_method
  :: HTMLFormElement -> (IO DOMString)
foreign import javascript unsafe "$1.method = $2" js_set_method
  :: HTMLFormElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.name" js_get_name
  :: HTMLFormElement -> (IO DOMString)
foreign import javascript unsafe "$1.name = $2" js_set_name
  :: HTMLFormElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.noValidate" js_get_noValidate
  :: HTMLFormElement -> (IO Bool)
foreign import javascript unsafe "$1.noValidate = $2" js_set_noValidate
  :: HTMLFormElement -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.target" js_get_target
  :: HTMLFormElement -> (IO DOMString)
foreign import javascript unsafe "$1.target = $2" js_set_target
  :: HTMLFormElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.elements" js_get_elements
  :: HTMLFormElement -> (IO HTMLCollection)
foreign import javascript unsafe "$1.length" js_get_length
  :: HTMLFormElement -> (IO Int32)
