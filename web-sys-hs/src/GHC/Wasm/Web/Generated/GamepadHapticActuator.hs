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
module GHC.Wasm.Web.Generated.GamepadHapticActuator (
        GamepadHapticActuator, GamepadHapticActuatorClass,
        js_fun_pulse_double_double_Promise_boolean, js_get_type
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.GamepadHapticActuator.Core
import GHC.Wasm.Web.Generated.GamepadHapticActuatorType.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.pulse($2,$3)" js_fun_pulse_double_double_Promise_boolean
  :: GamepadHapticActuator
     -> (Double -> (Double -> (IO (Promise (JSPrimClass Bool)))))
foreign import javascript unsafe "$1.type" js_get_type
  :: GamepadHapticActuator -> (IO GamepadHapticActuatorType)
