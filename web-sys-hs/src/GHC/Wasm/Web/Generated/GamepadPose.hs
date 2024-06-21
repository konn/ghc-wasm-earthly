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
module GHC.Wasm.Web.Generated.GamepadPose (
        GamepadPose, GamepadPoseClass, js_get_hasOrientation,
        js_get_hasPosition, js_get_position, js_get_linearVelocity,
        js_get_linearAcceleration, js_get_orientation,
        js_get_angularVelocity, js_get_angularAcceleration
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.GamepadPose.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.hasOrientation" js_get_hasOrientation
  :: GamepadPose -> (IO Bool)
foreign import javascript unsafe "$1.hasPosition" js_get_hasPosition
  :: GamepadPose -> (IO Bool)
foreign import javascript unsafe "$1.position" js_get_position
  :: GamepadPose -> (IO (Nullable (JSByteArrayClass Float)))
foreign import javascript unsafe "$1.linearVelocity" js_get_linearVelocity
  :: GamepadPose -> (IO (Nullable (JSByteArrayClass Float)))
foreign import javascript unsafe "$1.linearAcceleration" js_get_linearAcceleration
  :: GamepadPose -> (IO (Nullable (JSByteArrayClass Float)))
foreign import javascript unsafe "$1.orientation" js_get_orientation
  :: GamepadPose -> (IO (Nullable (JSByteArrayClass Float)))
foreign import javascript unsafe "$1.angularVelocity" js_get_angularVelocity
  :: GamepadPose -> (IO (Nullable (JSByteArrayClass Float)))
foreign import javascript unsafe "$1.angularAcceleration" js_get_angularAcceleration
  :: GamepadPose -> (IO (Nullable (JSByteArrayClass Float)))
