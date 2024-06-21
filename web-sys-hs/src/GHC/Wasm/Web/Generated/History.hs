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
module GHC.Wasm.Web.Generated.History (
        History, HistoryClass, js_fun_go_nullable_long_undefined,
        js_fun_back__undefined, js_fun_forward__undefined,
        js_fun_pushState_any_DOMString_nullable_nullable_DOMString_undefined,
        js_fun_replaceState_any_DOMString_nullable_nullable_DOMString_undefined,
        js_get_length, js_get_scrollRestoration, js_set_scrollRestoration,
        js_get_state
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.History.Core
import GHC.Wasm.Web.Generated.ScrollRestoration.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.go($2)" js_fun_go_nullable_long_undefined
  :: History -> (Nullable (JSPrimClass Int32) -> (IO ()))
foreign import javascript unsafe "$1.back()" js_fun_back__undefined
  :: History -> (IO ())
foreign import javascript unsafe "$1.forward()" js_fun_forward__undefined
  :: History -> (IO ())
foreign import javascript unsafe "$1.pushState($2,$3,$4)" js_fun_pushState_any_DOMString_nullable_nullable_DOMString_undefined
  :: History
     -> (JSAny
         -> (DOMString
             -> (Nullable (NullableClass DOMStringClass) -> (IO ()))))
foreign import javascript unsafe "$1.replaceState($2,$3,$4)" js_fun_replaceState_any_DOMString_nullable_nullable_DOMString_undefined
  :: History
     -> (JSAny
         -> (DOMString
             -> (Nullable (NullableClass DOMStringClass) -> (IO ()))))
foreign import javascript unsafe "$1.length" js_get_length
  :: History -> (IO Word32)
foreign import javascript unsafe "$1.scrollRestoration" js_get_scrollRestoration
  :: History -> (IO ScrollRestoration)
foreign import javascript unsafe "$1.scrollRestoration = $2" js_set_scrollRestoration
  :: History -> (ScrollRestoration -> (IO ()))
foreign import javascript unsafe "$1.state" js_get_state
  :: History -> (IO JSAny)
