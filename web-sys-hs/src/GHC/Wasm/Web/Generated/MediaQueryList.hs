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
module GHC.Wasm.Web.Generated.MediaQueryList (
        MediaQueryList, MediaQueryListClass,
        js_fun_addListener_nullable_EventListener_undefined,
        js_fun_removeListener_nullable_EventListener_undefined,
        js_get_media, js_get_matches, js_get_onchange, js_set_onchange
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventListener.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.MediaQueryList.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.addListener($2)" js_fun_addListener_nullable_EventListener_undefined
  :: MediaQueryList -> (Nullable EventListenerClass -> (IO ()))
foreign import javascript unsafe "$1.removeListener($2)" js_fun_removeListener_nullable_EventListener_undefined
  :: MediaQueryList -> (Nullable EventListenerClass -> (IO ()))
foreign import javascript unsafe "$1.media" js_get_media
  :: MediaQueryList -> (IO DOMString)
foreign import javascript unsafe "$1.matches" js_get_matches
  :: MediaQueryList -> (IO Bool)
foreign import javascript unsafe "$1.onchange" js_get_onchange
  :: MediaQueryList -> (IO EventHandler)
foreign import javascript unsafe "$1.onchange = $2" js_set_onchange
  :: MediaQueryList -> (EventHandler -> (IO ()))
