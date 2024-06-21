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
module GHC.Wasm.Web.Generated.Gamepad (
        Gamepad, GamepadClass, js_get_id, js_get_index, js_get_mapping,
        js_get_hand, js_get_displayId, js_get_connected, js_get_buttons,
        js_get_axes, js_get_timestamp, js_get_pose, js_get_hapticActuators
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMHighResTimeStamp.Core
import GHC.Wasm.Web.Generated.Gamepad.Core
import GHC.Wasm.Web.Generated.GamepadButton.Core
import GHC.Wasm.Web.Generated.GamepadHand.Core
import GHC.Wasm.Web.Generated.GamepadHapticActuator.Core
import GHC.Wasm.Web.Generated.GamepadMappingType.Core
import GHC.Wasm.Web.Generated.GamepadPose.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.id" js_get_id
  :: Gamepad -> (IO DOMString)
foreign import javascript unsafe "$1.index" js_get_index
  :: Gamepad -> (IO Word32)
foreign import javascript unsafe "$1.mapping" js_get_mapping
  :: Gamepad -> (IO GamepadMappingType)
foreign import javascript unsafe "$1.hand" js_get_hand
  :: Gamepad -> (IO GamepadHand)
foreign import javascript unsafe "$1.displayId" js_get_displayId
  :: Gamepad -> (IO Word32)
foreign import javascript unsafe "$1.connected" js_get_connected
  :: Gamepad -> (IO Bool)
foreign import javascript unsafe "$1.buttons" js_get_buttons
  :: Gamepad -> (IO (Sequence GamepadButtonClass))
foreign import javascript unsafe "$1.axes" js_get_axes
  :: Gamepad -> (IO (Sequence (JSPrimClass Double)))
foreign import javascript unsafe "$1.timestamp" js_get_timestamp
  :: Gamepad -> (IO DOMHighResTimeStamp)
foreign import javascript unsafe "$1.pose" js_get_pose
  :: Gamepad -> (IO (Nullable GamepadPoseClass))
foreign import javascript unsafe "$1.hapticActuators" js_get_hapticActuators
  :: Gamepad -> (IO (Sequence GamepadHapticActuatorClass))
