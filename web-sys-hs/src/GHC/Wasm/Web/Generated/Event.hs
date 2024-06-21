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
module GHC.Wasm.Web.Generated.Event (
        Event, EventClass, js_cons_Event, js_const_Event_NONE,
        js_const_Event_CAPTURING_PHASE, js_const_Event_AT_TARGET,
        js_const_Event_BUBBLING_PHASE,
        js_fun_composedPath__sequence_EventTarget,
        js_fun_stopPropagation__undefined,
        js_fun_stopImmediatePropagation__undefined,
        js_fun_preventDefault__undefined,
        js_fun_initEvent_DOMString_nullable_boolean_nullable_boolean_undefined,
        js_get_type, js_get_target, js_get_currentTarget,
        js_get_eventPhase, js_get_bubbles, js_get_cancelable,
        js_get_defaultPrevented, js_get_defaultPreventedByChrome,
        js_get_defaultPreventedByContent, js_get_composed,
        js_get_isTrusted, js_get_timeStamp, js_get_cancelBubble,
        js_set_cancelBubble
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMHighResTimeStamp.Core
import GHC.Wasm.Web.Generated.Event.Core
import GHC.Wasm.Web.Generated.EventInit.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new Event($1,$2)" js_cons_Event
  :: DOMString -> (Nullable EventInitClass -> (IO Event))
js_const_Event_NONE :: Word16
js_const_Event_NONE = 0
js_const_Event_CAPTURING_PHASE :: Word16
js_const_Event_CAPTURING_PHASE = 1
js_const_Event_AT_TARGET :: Word16
js_const_Event_AT_TARGET = 2
js_const_Event_BUBBLING_PHASE :: Word16
js_const_Event_BUBBLING_PHASE = 3
foreign import javascript unsafe "$1.composedPath()" js_fun_composedPath__sequence_EventTarget
  :: Event -> (IO (Sequence EventTargetClass))
foreign import javascript unsafe "$1.stopPropagation()" js_fun_stopPropagation__undefined
  :: Event -> (IO ())
foreign import javascript unsafe "$1.stopImmediatePropagation()" js_fun_stopImmediatePropagation__undefined
  :: Event -> (IO ())
foreign import javascript unsafe "$1.preventDefault()" js_fun_preventDefault__undefined
  :: Event -> (IO ())
foreign import javascript unsafe "$1.initEvent($2,$3,$4)" js_fun_initEvent_DOMString_nullable_boolean_nullable_boolean_undefined
  :: Event
     -> (DOMString
         -> (Nullable (JSPrimClass Bool)
             -> (Nullable (JSPrimClass Bool) -> (IO ()))))
foreign import javascript unsafe "$1.type" js_get_type
  :: Event -> (IO DOMString)
foreign import javascript unsafe "$1.target" js_get_target
  :: Event -> (IO (Nullable EventTargetClass))
foreign import javascript unsafe "$1.currentTarget" js_get_currentTarget
  :: Event -> (IO (Nullable EventTargetClass))
foreign import javascript unsafe "$1.eventPhase" js_get_eventPhase
  :: Event -> (IO Word16)
foreign import javascript unsafe "$1.bubbles" js_get_bubbles
  :: Event -> (IO Bool)
foreign import javascript unsafe "$1.cancelable" js_get_cancelable
  :: Event -> (IO Bool)
foreign import javascript unsafe "$1.defaultPrevented" js_get_defaultPrevented
  :: Event -> (IO Bool)
foreign import javascript unsafe "$1.defaultPreventedByChrome" js_get_defaultPreventedByChrome
  :: Event -> (IO Bool)
foreign import javascript unsafe "$1.defaultPreventedByContent" js_get_defaultPreventedByContent
  :: Event -> (IO Bool)
foreign import javascript unsafe "$1.composed" js_get_composed
  :: Event -> (IO Bool)
foreign import javascript unsafe "$1.isTrusted" js_get_isTrusted
  :: Event -> (IO Bool)
foreign import javascript unsafe "$1.timeStamp" js_get_timeStamp
  :: Event -> (IO DOMHighResTimeStamp)
foreign import javascript unsafe "$1.cancelBubble" js_get_cancelBubble
  :: Event -> (IO Bool)
foreign import javascript unsafe "$1.cancelBubble = $2" js_set_cancelBubble
  :: Event -> (Bool -> (IO ()))
