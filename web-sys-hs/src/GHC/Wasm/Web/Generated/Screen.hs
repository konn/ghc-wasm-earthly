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
module GHC.Wasm.Web.Generated.Screen (
        Screen, ScreenClass, js_get_availWidth, js_get_availHeight,
        js_get_width, js_get_height, js_get_colorDepth, js_get_pixelDepth,
        js_get_top, js_get_left, js_get_availTop, js_get_availLeft,
        js_get_orientation, js_get_colorGamut, js_get_luminance,
        js_get_onchange, js_set_onchange
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.Screen.Core
import GHC.Wasm.Web.Generated.ScreenColorGamut.Core
import GHC.Wasm.Web.Generated.ScreenLuminance.Core
import GHC.Wasm.Web.Generated.ScreenOrientation.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.availWidth" js_get_availWidth
  :: Screen -> (IO Int32)
foreign import javascript unsafe "$1.availHeight" js_get_availHeight
  :: Screen -> (IO Int32)
foreign import javascript unsafe "$1.width" js_get_width
  :: Screen -> (IO Int32)
foreign import javascript unsafe "$1.height" js_get_height
  :: Screen -> (IO Int32)
foreign import javascript unsafe "$1.colorDepth" js_get_colorDepth
  :: Screen -> (IO Int32)
foreign import javascript unsafe "$1.pixelDepth" js_get_pixelDepth
  :: Screen -> (IO Int32)
foreign import javascript unsafe "$1.top" js_get_top
  :: Screen -> (IO Int32)
foreign import javascript unsafe "$1.left" js_get_left
  :: Screen -> (IO Int32)
foreign import javascript unsafe "$1.availTop" js_get_availTop
  :: Screen -> (IO Int32)
foreign import javascript unsafe "$1.availLeft" js_get_availLeft
  :: Screen -> (IO Int32)
foreign import javascript unsafe "$1.orientation" js_get_orientation
  :: Screen -> (IO ScreenOrientation)
foreign import javascript unsafe "$1.colorGamut" js_get_colorGamut
  :: Screen -> (IO ScreenColorGamut)
foreign import javascript unsafe "$1.luminance" js_get_luminance
  :: Screen -> (IO (Nullable ScreenLuminanceClass))
foreign import javascript unsafe "$1.onchange" js_get_onchange
  :: Screen -> (IO EventHandler)
foreign import javascript unsafe "$1.onchange = $2" js_set_onchange
  :: Screen -> (EventHandler -> (IO ()))
