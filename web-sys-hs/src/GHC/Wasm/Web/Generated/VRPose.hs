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
module GHC.Wasm.Web.Generated.VRPose (
        VRPose, VRPoseClass, js_get_position, js_get_linearVelocity,
        js_get_linearAcceleration, js_get_orientation,
        js_get_angularVelocity, js_get_angularAcceleration
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.VRPose.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.position" js_get_position
  :: VRPose -> (IO (Nullable (JSByteArrayClass Float)))
foreign import javascript unsafe "$1.linearVelocity" js_get_linearVelocity
  :: VRPose -> (IO (Nullable (JSByteArrayClass Float)))
foreign import javascript unsafe "$1.linearAcceleration" js_get_linearAcceleration
  :: VRPose -> (IO (Nullable (JSByteArrayClass Float)))
foreign import javascript unsafe "$1.orientation" js_get_orientation
  :: VRPose -> (IO (Nullable (JSByteArrayClass Float)))
foreign import javascript unsafe "$1.angularVelocity" js_get_angularVelocity
  :: VRPose -> (IO (Nullable (JSByteArrayClass Float)))
foreign import javascript unsafe "$1.angularAcceleration" js_get_angularAcceleration
  :: VRPose -> (IO (Nullable (JSByteArrayClass Float)))
