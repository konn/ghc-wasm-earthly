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
module GHC.Wasm.Web.Generated.DOMTokenList (
        DOMTokenList, DOMTokenListClass, js_fun_contains_DOMString_boolean,
        js_fun_add_DOMString_undefined, js_fun_remove_DOMString_undefined,
        js_fun_replace_DOMString_DOMString_boolean,
        js_fun_toggle_DOMString_nullable_boolean_boolean,
        js_fun_supports_DOMString_boolean, js_get_length, js_get_value,
        js_set_value, js_iter_DOMTokenList_DOMString
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMTokenList.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.contains($2)" js_fun_contains_DOMString_boolean
  :: DOMTokenList -> (DOMString -> (IO Bool))
foreign import javascript unsafe "$1.add(... $2)" js_fun_add_DOMString_undefined
  :: DOMTokenList -> (FrozenArray DOMStringClass -> (IO ()))
foreign import javascript unsafe "$1.remove(... $2)" js_fun_remove_DOMString_undefined
  :: DOMTokenList -> (FrozenArray DOMStringClass -> (IO ()))
foreign import javascript unsafe "$1.replace($2,$3)" js_fun_replace_DOMString_DOMString_boolean
  :: DOMTokenList -> (DOMString -> (DOMString -> (IO Bool)))
foreign import javascript unsafe "$1.toggle($2,$3)" js_fun_toggle_DOMString_nullable_boolean_boolean
  :: DOMTokenList
     -> (DOMString -> (Nullable (JSPrimClass Bool) -> (IO Bool)))
foreign import javascript unsafe "$1.supports($2)" js_fun_supports_DOMString_boolean
  :: DOMTokenList -> (DOMString -> (IO Bool))
foreign import javascript unsafe "$1.length" js_get_length
  :: DOMTokenList -> (IO Word32)
foreign import javascript unsafe "$1.value" js_get_value
  :: DOMTokenList -> (IO DOMString)
foreign import javascript unsafe "$1.value = $2" js_set_value
  :: DOMTokenList -> (DOMString -> (IO ()))
js_iter_DOMTokenList_DOMString ::
  Iterable DOMStringClass -> DOMTokenList
js_iter_DOMTokenList_DOMString = unsafeCast
