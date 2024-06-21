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
module GHC.Wasm.Web.Generated.SVGPreserveAspectRatio (
        SVGPreserveAspectRatio, SVGPreserveAspectRatioClass,
        js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_UNKNOWN,
        js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_NONE,
        js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMINYMIN,
        js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMIDYMIN,
        js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMAXYMIN,
        js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMINYMID,
        js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMIDYMID,
        js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMAXYMID,
        js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMINYMAX,
        js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMIDYMAX,
        js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMAXYMAX,
        js_const_SVGPreserveAspectRatio_SVG_MEETORSLICE_UNKNOWN,
        js_const_SVGPreserveAspectRatio_SVG_MEETORSLICE_MEET,
        js_const_SVGPreserveAspectRatio_SVG_MEETORSLICE_SLICE,
        js_get_align, js_set_align, js_get_meetOrSlice, js_set_meetOrSlice
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.SVGPreserveAspectRatio.Core
import GHC.Wasm.Web.Types
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_UNKNOWN ::
  Word16
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_UNKNOWN = 0
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_NONE ::
  Word16
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_NONE = 1
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMINYMIN ::
  Word16
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMINYMIN
  = 2
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMIDYMIN ::
  Word16
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMIDYMIN
  = 3
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMAXYMIN ::
  Word16
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMAXYMIN
  = 4
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMINYMID ::
  Word16
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMINYMID
  = 5
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMIDYMID ::
  Word16
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMIDYMID
  = 6
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMAXYMID ::
  Word16
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMAXYMID
  = 7
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMINYMAX ::
  Word16
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMINYMAX
  = 8
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMIDYMAX ::
  Word16
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMIDYMAX
  = 9
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMAXYMAX ::
  Word16
js_const_SVGPreserveAspectRatio_SVG_PRESERVEASPECTRATIO_XMAXYMAX
  = 10
js_const_SVGPreserveAspectRatio_SVG_MEETORSLICE_UNKNOWN :: Word16
js_const_SVGPreserveAspectRatio_SVG_MEETORSLICE_UNKNOWN = 0
js_const_SVGPreserveAspectRatio_SVG_MEETORSLICE_MEET :: Word16
js_const_SVGPreserveAspectRatio_SVG_MEETORSLICE_MEET = 1
js_const_SVGPreserveAspectRatio_SVG_MEETORSLICE_SLICE :: Word16
js_const_SVGPreserveAspectRatio_SVG_MEETORSLICE_SLICE = 2
foreign import javascript unsafe "$1.align" js_get_align
  :: SVGPreserveAspectRatio -> (IO Word16)
foreign import javascript unsafe "$1.align = $2" js_set_align
  :: SVGPreserveAspectRatio -> (Word16 -> (IO ()))
foreign import javascript unsafe "$1.meetOrSlice" js_get_meetOrSlice
  :: SVGPreserveAspectRatio -> (IO Word16)
foreign import javascript unsafe "$1.meetOrSlice = $2" js_set_meetOrSlice
  :: SVGPreserveAspectRatio -> (Word16 -> (IO ()))
