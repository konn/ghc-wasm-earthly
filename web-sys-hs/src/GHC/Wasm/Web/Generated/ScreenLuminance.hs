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
module GHC.Wasm.Web.Generated.ScreenLuminance (
        ScreenLuminance, ScreenLuminanceClass, js_get_min, js_get_max,
        js_get_maxAverage
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ScreenLuminance.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.min" js_get_min
  :: ScreenLuminance -> (IO Double)
foreign import javascript unsafe "$1.max" js_get_max
  :: ScreenLuminance -> (IO Double)
foreign import javascript unsafe "$1.maxAverage" js_get_maxAverage
  :: ScreenLuminance -> (IO Double)
