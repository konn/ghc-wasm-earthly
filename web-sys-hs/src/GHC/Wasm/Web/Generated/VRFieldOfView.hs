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
module GHC.Wasm.Web.Generated.VRFieldOfView (
        VRFieldOfView, VRFieldOfViewClass, js_get_upDegrees,
        js_get_rightDegrees, js_get_downDegrees, js_get_leftDegrees
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.VRFieldOfView.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.upDegrees" js_get_upDegrees
  :: VRFieldOfView -> (IO Double)
foreign import javascript unsafe "$1.rightDegrees" js_get_rightDegrees
  :: VRFieldOfView -> (IO Double)
foreign import javascript unsafe "$1.downDegrees" js_get_downDegrees
  :: VRFieldOfView -> (IO Double)
foreign import javascript unsafe "$1.leftDegrees" js_get_leftDegrees
  :: VRFieldOfView -> (IO Double)
