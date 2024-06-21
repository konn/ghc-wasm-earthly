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
module GHC.Wasm.Web.Generated.VisualViewport (
        VisualViewport, VisualViewportClass, js_get_offsetLeft,
        js_get_offsetTop, js_get_pageLeft, js_get_pageTop, js_get_width,
        js_get_height, js_get_scale, js_get_onresize, js_set_onresize,
        js_get_onscroll, js_set_onscroll, js_get_onscrollend,
        js_set_onscrollend
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.VisualViewport.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.offsetLeft" js_get_offsetLeft
  :: VisualViewport -> (IO Double)
foreign import javascript unsafe "$1.offsetTop" js_get_offsetTop
  :: VisualViewport -> (IO Double)
foreign import javascript unsafe "$1.pageLeft" js_get_pageLeft
  :: VisualViewport -> (IO Double)
foreign import javascript unsafe "$1.pageTop" js_get_pageTop
  :: VisualViewport -> (IO Double)
foreign import javascript unsafe "$1.width" js_get_width
  :: VisualViewport -> (IO Double)
foreign import javascript unsafe "$1.height" js_get_height
  :: VisualViewport -> (IO Double)
foreign import javascript unsafe "$1.scale" js_get_scale
  :: VisualViewport -> (IO Double)
foreign import javascript unsafe "$1.onresize" js_get_onresize
  :: VisualViewport -> (IO EventHandler)
foreign import javascript unsafe "$1.onresize = $2" js_set_onresize
  :: VisualViewport -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onscroll" js_get_onscroll
  :: VisualViewport -> (IO EventHandler)
foreign import javascript unsafe "$1.onscroll = $2" js_set_onscroll
  :: VisualViewport -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onscrollend" js_get_onscrollend
  :: VisualViewport -> (IO EventHandler)
foreign import javascript unsafe "$1.onscrollend = $2" js_set_onscrollend
  :: VisualViewport -> (EventHandler -> (IO ()))
