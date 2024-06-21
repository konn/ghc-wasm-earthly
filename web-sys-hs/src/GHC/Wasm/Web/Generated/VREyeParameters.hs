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
module GHC.Wasm.Web.Generated.VREyeParameters (
        VREyeParameters, VREyeParametersClass, js_get_offset,
        js_get_fieldOfView, js_get_renderWidth, js_get_renderHeight
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.VREyeParameters.Core
import GHC.Wasm.Web.Generated.VRFieldOfView.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.offset" js_get_offset
  :: VREyeParameters -> (IO (JSByteArray Float))
foreign import javascript unsafe "$1.fieldOfView" js_get_fieldOfView
  :: VREyeParameters -> (IO VRFieldOfView)
foreign import javascript unsafe "$1.renderWidth" js_get_renderWidth
  :: VREyeParameters -> (IO Word32)
foreign import javascript unsafe "$1.renderHeight" js_get_renderHeight
  :: VREyeParameters -> (IO Word32)
