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
module GHC.Wasm.Web.Generated.VTTRegion (
        VTTRegion, VTTRegionClass, js_get_id, js_set_id, js_get_width,
        js_set_width, js_get_lines, js_set_lines, js_get_regionAnchorX,
        js_set_regionAnchorX, js_get_regionAnchorY, js_set_regionAnchorY,
        js_get_viewportAnchorX, js_set_viewportAnchorX,
        js_get_viewportAnchorY, js_set_viewportAnchorY, js_get_scroll,
        js_set_scroll
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ScrollSetting.Core
import GHC.Wasm.Web.Generated.VTTRegion.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.id" js_get_id
  :: VTTRegion -> (IO DOMString)
foreign import javascript unsafe "$1.id = $2" js_set_id
  :: VTTRegion -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.width" js_get_width
  :: VTTRegion -> (IO Double)
foreign import javascript unsafe "$1.width = $2" js_set_width
  :: VTTRegion -> (Double -> (IO ()))
foreign import javascript unsafe "$1.lines" js_get_lines
  :: VTTRegion -> (IO Int32)
foreign import javascript unsafe "$1.lines = $2" js_set_lines
  :: VTTRegion -> (Int32 -> (IO ()))
foreign import javascript unsafe "$1.regionAnchorX" js_get_regionAnchorX
  :: VTTRegion -> (IO Double)
foreign import javascript unsafe "$1.regionAnchorX = $2" js_set_regionAnchorX
  :: VTTRegion -> (Double -> (IO ()))
foreign import javascript unsafe "$1.regionAnchorY" js_get_regionAnchorY
  :: VTTRegion -> (IO Double)
foreign import javascript unsafe "$1.regionAnchorY = $2" js_set_regionAnchorY
  :: VTTRegion -> (Double -> (IO ()))
foreign import javascript unsafe "$1.viewportAnchorX" js_get_viewportAnchorX
  :: VTTRegion -> (IO Double)
foreign import javascript unsafe "$1.viewportAnchorX = $2" js_set_viewportAnchorX
  :: VTTRegion -> (Double -> (IO ()))
foreign import javascript unsafe "$1.viewportAnchorY" js_get_viewportAnchorY
  :: VTTRegion -> (IO Double)
foreign import javascript unsafe "$1.viewportAnchorY = $2" js_set_viewportAnchorY
  :: VTTRegion -> (Double -> (IO ()))
foreign import javascript unsafe "$1.scroll" js_get_scroll
  :: VTTRegion -> (IO ScrollSetting)
foreign import javascript unsafe "$1.scroll = $2" js_set_scroll
  :: VTTRegion -> (ScrollSetting -> (IO ()))
