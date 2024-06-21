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
module GHC.Wasm.Web.Generated.VRStageParameters (
        VRStageParameters, VRStageParametersClass,
        js_get_sittingToStandingTransform, js_get_sizeX, js_get_sizeZ
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.VRStageParameters.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.sittingToStandingTransform" js_get_sittingToStandingTransform
  :: VRStageParameters -> (IO (JSByteArray Float))
foreign import javascript unsafe "$1.sizeX" js_get_sizeX
  :: VRStageParameters -> (IO Float)
foreign import javascript unsafe "$1.sizeZ" js_get_sizeZ
  :: VRStageParameters -> (IO Float)
