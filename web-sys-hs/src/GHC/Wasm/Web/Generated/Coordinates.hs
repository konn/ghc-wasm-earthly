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
module GHC.Wasm.Web.Generated.Coordinates (
        Coordinates, CoordinatesClass, js_get_latitude, js_get_longitude,
        js_get_altitude, js_get_accuracy, js_get_altitudeAccuracy,
        js_get_heading, js_get_speed
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Coordinates.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.latitude" js_get_latitude
  :: Coordinates -> (IO Double)
foreign import javascript unsafe "$1.longitude" js_get_longitude
  :: Coordinates -> (IO Double)
foreign import javascript unsafe "$1.altitude" js_get_altitude
  :: Coordinates -> (IO (Nullable (JSPrimClass Double)))
foreign import javascript unsafe "$1.accuracy" js_get_accuracy
  :: Coordinates -> (IO Double)
foreign import javascript unsafe "$1.altitudeAccuracy" js_get_altitudeAccuracy
  :: Coordinates -> (IO (Nullable (JSPrimClass Double)))
foreign import javascript unsafe "$1.heading" js_get_heading
  :: Coordinates -> (IO (Nullable (JSPrimClass Double)))
foreign import javascript unsafe "$1.speed" js_get_speed
  :: Coordinates -> (IO (Nullable (JSPrimClass Double)))
