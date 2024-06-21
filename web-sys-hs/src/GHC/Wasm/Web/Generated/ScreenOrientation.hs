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
module GHC.Wasm.Web.Generated.ScreenOrientation (
        ScreenOrientation, ScreenOrientationClass,
        js_fun_lock_OrientationLockType_Promise_undefined,
        js_fun_unlock__undefined, js_get_type, js_get_angle,
        js_get_onchange, js_set_onchange
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.OrientationLockType.Core
import GHC.Wasm.Web.Generated.OrientationType.Core
import GHC.Wasm.Web.Generated.ScreenOrientation.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.lock($2)" js_fun_lock_OrientationLockType_Promise_undefined
  :: ScreenOrientation
     -> (OrientationLockType -> (IO (Promise UndefinedClass)))
foreign import javascript unsafe "$1.unlock()" js_fun_unlock__undefined
  :: ScreenOrientation -> (IO ())
foreign import javascript unsafe "$1.type" js_get_type
  :: ScreenOrientation -> (IO OrientationType)
foreign import javascript unsafe "$1.angle" js_get_angle
  :: ScreenOrientation -> (IO Word16)
foreign import javascript unsafe "$1.onchange" js_get_onchange
  :: ScreenOrientation -> (IO EventHandler)
foreign import javascript unsafe "$1.onchange = $2" js_set_onchange
  :: ScreenOrientation -> (EventHandler -> (IO ()))
