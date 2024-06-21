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
module GHC.Wasm.Web.Generated.VTTCue (
        VTTCue, VTTCueClass, js_cons_VTTCue,
        js_fun_getCueAsHTML__DocumentFragment, js_get_region,
        js_set_region, js_get_vertical, js_set_vertical,
        js_get_snapToLines, js_set_snapToLines, js_get_line, js_set_line,
        js_get_lineAlign, js_set_lineAlign, js_get_position,
        js_set_position, js_get_positionAlign, js_set_positionAlign,
        js_get_size, js_set_size, js_get_align, js_set_align, js_get_text,
        js_set_text
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AlignSetting.Core
import GHC.Wasm.Web.Generated.AutoKeyword.Core
import GHC.Wasm.Web.Generated.DirectionSetting.Core
import GHC.Wasm.Web.Generated.DocumentFragment.Core
import GHC.Wasm.Web.Generated.LineAlignSetting.Core
import GHC.Wasm.Web.Generated.PositionAlignSetting.Core
import GHC.Wasm.Web.Generated.TextTrackCue.Core
import GHC.Wasm.Web.Generated.VTTCue.Core
import GHC.Wasm.Web.Generated.VTTRegion.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new VTTCue($1,$2,$3)" js_cons_VTTCue
  :: Double -> (Double -> (DOMString -> (IO VTTCue)))
foreign import javascript unsafe "$1.getCueAsHTML()" js_fun_getCueAsHTML__DocumentFragment
  :: VTTCue -> (IO DocumentFragment)
foreign import javascript unsafe "$1.region" js_get_region
  :: VTTCue -> (IO (Nullable VTTRegionClass))
foreign import javascript unsafe "$1.region = $2" js_set_region
  :: VTTCue -> (Nullable VTTRegionClass -> (IO ()))
foreign import javascript unsafe "$1.vertical" js_get_vertical
  :: VTTCue -> (IO DirectionSetting)
foreign import javascript unsafe "$1.vertical = $2" js_set_vertical
  :: VTTCue -> (DirectionSetting -> (IO ()))
foreign import javascript unsafe "$1.snapToLines" js_get_snapToLines
  :: VTTCue -> (IO Bool)
foreign import javascript unsafe "$1.snapToLines = $2" js_set_snapToLines
  :: VTTCue -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.line" js_get_line
  :: VTTCue
     -> (IO (JSObject (UnionClass '[JSPrimClass Double,
                                    AutoKeywordClass])))
foreign import javascript unsafe "$1.line = $2" js_set_line
  :: VTTCue
     -> (JSObject (UnionClass '[JSPrimClass Double, AutoKeywordClass])
         -> (IO ()))
foreign import javascript unsafe "$1.lineAlign" js_get_lineAlign
  :: VTTCue -> (IO LineAlignSetting)
foreign import javascript unsafe "$1.lineAlign = $2" js_set_lineAlign
  :: VTTCue -> (LineAlignSetting -> (IO ()))
foreign import javascript unsafe "$1.position" js_get_position
  :: VTTCue
     -> (IO (JSObject (UnionClass '[JSPrimClass Double,
                                    AutoKeywordClass])))
foreign import javascript unsafe "$1.position = $2" js_set_position
  :: VTTCue
     -> (JSObject (UnionClass '[JSPrimClass Double, AutoKeywordClass])
         -> (IO ()))
foreign import javascript unsafe "$1.positionAlign" js_get_positionAlign
  :: VTTCue -> (IO PositionAlignSetting)
foreign import javascript unsafe "$1.positionAlign = $2" js_set_positionAlign
  :: VTTCue -> (PositionAlignSetting -> (IO ()))
foreign import javascript unsafe "$1.size" js_get_size
  :: VTTCue -> (IO Double)
foreign import javascript unsafe "$1.size = $2" js_set_size
  :: VTTCue -> (Double -> (IO ()))
foreign import javascript unsafe "$1.align" js_get_align
  :: VTTCue -> (IO AlignSetting)
foreign import javascript unsafe "$1.align = $2" js_set_align
  :: VTTCue -> (AlignSetting -> (IO ()))
foreign import javascript unsafe "$1.text" js_get_text
  :: VTTCue -> (IO DOMString)
foreign import javascript unsafe "$1.text = $2" js_set_text
  :: VTTCue -> (DOMString -> (IO ()))
