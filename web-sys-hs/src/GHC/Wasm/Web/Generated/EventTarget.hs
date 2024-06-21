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
module GHC.Wasm.Web.Generated.EventTarget (
        EventTarget, EventTargetClass,
        js_fun_addEventListener_DOMString_EventListener_nullable_Union_AddEventListenerOptions_boolean_EndUnion_nullable_nullable_boolean_undefined,
        js_fun_removeEventListener_DOMString_EventListener_nullable_Union_EventListenerOptions_boolean_EndUnion_undefined,
        js_fun_dispatchEvent_Event_boolean
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AddEventListenerOptions.Core
import GHC.Wasm.Web.Generated.Event.Core
import GHC.Wasm.Web.Generated.EventListener.Core
import GHC.Wasm.Web.Generated.EventListenerOptions.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.addEventListener($2,$3,$4,$5)" js_fun_addEventListener_DOMString_EventListener_nullable_Union_AddEventListenerOptions_boolean_EndUnion_nullable_nullable_boolean_undefined
  :: EventTarget
     -> (DOMString
         -> (EventListener
             -> (Nullable (UnionClass '[AddEventListenerOptionsClass,
                                        JSPrimClass Bool])
                 -> (Nullable (NullableClass (JSPrimClass Bool)) -> (IO ())))))
foreign import javascript unsafe "$1.removeEventListener($2,$3,$4)" js_fun_removeEventListener_DOMString_EventListener_nullable_Union_EventListenerOptions_boolean_EndUnion_undefined
  :: EventTarget
     -> (DOMString
         -> (EventListener
             -> (Nullable (UnionClass '[EventListenerOptionsClass,
                                        JSPrimClass Bool])
                 -> (IO ()))))
foreign import javascript unsafe "$1.dispatchEvent($2)" js_fun_dispatchEvent_Event_boolean
  :: EventTarget -> (Event -> (IO Bool))
