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
module GHC.Wasm.Web.Generated.WakeLockSentinel (
        WakeLockSentinel, WakeLockSentinelClass,
        js_fun_release__Promise_undefined, js_get_released, js_get_type,
        js_get_onrelease, js_set_onrelease
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.WakeLockSentinel.Core
import GHC.Wasm.Web.Generated.WakeLockType.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.release()" js_fun_release__Promise_undefined
  :: WakeLockSentinel -> (IO (Promise UndefinedClass))
foreign import javascript unsafe "$1.released" js_get_released
  :: WakeLockSentinel -> (IO Bool)
foreign import javascript unsafe "$1.type" js_get_type
  :: WakeLockSentinel -> (IO WakeLockType)
foreign import javascript unsafe "$1.onrelease" js_get_onrelease
  :: WakeLockSentinel -> (IO EventHandler)
foreign import javascript unsafe "$1.onrelease = $2" js_set_onrelease
  :: WakeLockSentinel -> (EventHandler -> (IO ()))
