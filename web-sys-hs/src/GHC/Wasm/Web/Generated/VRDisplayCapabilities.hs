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
module GHC.Wasm.Web.Generated.VRDisplayCapabilities (
        VRDisplayCapabilities, VRDisplayCapabilitiesClass,
        js_get_hasPosition, js_get_hasOrientation,
        js_get_hasExternalDisplay, js_get_canPresent, js_get_maxLayers
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.VRDisplayCapabilities.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.hasPosition" js_get_hasPosition
  :: VRDisplayCapabilities -> (IO Bool)
foreign import javascript unsafe "$1.hasOrientation" js_get_hasOrientation
  :: VRDisplayCapabilities -> (IO Bool)
foreign import javascript unsafe "$1.hasExternalDisplay" js_get_hasExternalDisplay
  :: VRDisplayCapabilities -> (IO Bool)
foreign import javascript unsafe "$1.canPresent" js_get_canPresent
  :: VRDisplayCapabilities -> (IO Bool)
foreign import javascript unsafe "$1.maxLayers" js_get_maxLayers
  :: VRDisplayCapabilities -> (IO Word32)
