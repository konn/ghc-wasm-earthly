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
module GHC.Wasm.Web.Generated.HTMLImageElement (
        HTMLImageElement, HTMLImageElementClass,
        js_fun_decode__Promise_undefined, js_get_alt, js_set_alt,
        js_get_src, js_set_src, js_get_srcset, js_set_srcset,
        js_get_crossOrigin, js_set_crossOrigin, js_get_useMap,
        js_set_useMap, js_get_referrerPolicy, js_set_referrerPolicy,
        js_get_isMap, js_set_isMap, js_get_width, js_set_width,
        js_get_height, js_set_height, js_get_decoding, js_set_decoding,
        js_get_naturalWidth, js_get_naturalHeight, js_get_complete,
        js_get_name, js_set_name, js_get_align, js_set_align,
        js_get_hspace, js_set_hspace, js_get_vspace, js_set_vspace,
        js_get_longDesc, js_set_longDesc, js_get_border, js_set_border,
        js_get_sizes, js_set_sizes, js_get_currentSrc
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.HTMLElement.Core
import GHC.Wasm.Web.Generated.HTMLImageElement.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.decode()" js_fun_decode__Promise_undefined
  :: HTMLImageElement -> (IO (Promise UndefinedClass))
foreign import javascript unsafe "$1.alt" js_get_alt
  :: HTMLImageElement -> (IO DOMString)
foreign import javascript unsafe "$1.alt = $2" js_set_alt
  :: HTMLImageElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.src" js_get_src
  :: HTMLImageElement -> (IO DOMString)
foreign import javascript unsafe "$1.src = $2" js_set_src
  :: HTMLImageElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.srcset" js_get_srcset
  :: HTMLImageElement -> (IO DOMString)
foreign import javascript unsafe "$1.srcset = $2" js_set_srcset
  :: HTMLImageElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.crossOrigin" js_get_crossOrigin
  :: HTMLImageElement -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.crossOrigin = $2" js_set_crossOrigin
  :: HTMLImageElement -> (Nullable DOMStringClass -> (IO ()))
foreign import javascript unsafe "$1.useMap" js_get_useMap
  :: HTMLImageElement -> (IO DOMString)
foreign import javascript unsafe "$1.useMap = $2" js_set_useMap
  :: HTMLImageElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.referrerPolicy" js_get_referrerPolicy
  :: HTMLImageElement -> (IO DOMString)
foreign import javascript unsafe "$1.referrerPolicy = $2" js_set_referrerPolicy
  :: HTMLImageElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.isMap" js_get_isMap
  :: HTMLImageElement -> (IO Bool)
foreign import javascript unsafe "$1.isMap = $2" js_set_isMap
  :: HTMLImageElement -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.width" js_get_width
  :: HTMLImageElement -> (IO Word32)
foreign import javascript unsafe "$1.width = $2" js_set_width
  :: HTMLImageElement -> (Word32 -> (IO ()))
foreign import javascript unsafe "$1.height" js_get_height
  :: HTMLImageElement -> (IO Word32)
foreign import javascript unsafe "$1.height = $2" js_set_height
  :: HTMLImageElement -> (Word32 -> (IO ()))
foreign import javascript unsafe "$1.decoding" js_get_decoding
  :: HTMLImageElement -> (IO DOMString)
foreign import javascript unsafe "$1.decoding = $2" js_set_decoding
  :: HTMLImageElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.naturalWidth" js_get_naturalWidth
  :: HTMLImageElement -> (IO Word32)
foreign import javascript unsafe "$1.naturalHeight" js_get_naturalHeight
  :: HTMLImageElement -> (IO Word32)
foreign import javascript unsafe "$1.complete" js_get_complete
  :: HTMLImageElement -> (IO Bool)
foreign import javascript unsafe "$1.name" js_get_name
  :: HTMLImageElement -> (IO DOMString)
foreign import javascript unsafe "$1.name = $2" js_set_name
  :: HTMLImageElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.align" js_get_align
  :: HTMLImageElement -> (IO DOMString)
foreign import javascript unsafe "$1.align = $2" js_set_align
  :: HTMLImageElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.hspace" js_get_hspace
  :: HTMLImageElement -> (IO Word32)
foreign import javascript unsafe "$1.hspace = $2" js_set_hspace
  :: HTMLImageElement -> (Word32 -> (IO ()))
foreign import javascript unsafe "$1.vspace" js_get_vspace
  :: HTMLImageElement -> (IO Word32)
foreign import javascript unsafe "$1.vspace = $2" js_set_vspace
  :: HTMLImageElement -> (Word32 -> (IO ()))
foreign import javascript unsafe "$1.longDesc" js_get_longDesc
  :: HTMLImageElement -> (IO DOMString)
foreign import javascript unsafe "$1.longDesc = $2" js_set_longDesc
  :: HTMLImageElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.border" js_get_border
  :: HTMLImageElement -> (IO DOMString)
foreign import javascript unsafe "$1.border = $2" js_set_border
  :: HTMLImageElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.sizes" js_get_sizes
  :: HTMLImageElement -> (IO DOMString)
foreign import javascript unsafe "$1.sizes = $2" js_set_sizes
  :: HTMLImageElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.currentSrc" js_get_currentSrc
  :: HTMLImageElement -> (IO DOMString)
