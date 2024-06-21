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
module GHC.Wasm.Web.Generated.FontFace (
        FontFace, FontFaceClass, js_cons_FontFace,
        js_fun_load__Promise_FontFace, js_get_family, js_set_family,
        js_get_style, js_set_style, js_get_weight, js_set_weight,
        js_get_stretch, js_set_stretch, js_get_unicodeRange,
        js_set_unicodeRange, js_get_variant, js_set_variant,
        js_get_featureSettings, js_set_featureSettings,
        js_get_variationSettings, js_set_variationSettings, js_get_display,
        js_set_display, js_get_status, js_get_loaded
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.BinaryData.Core
import GHC.Wasm.Web.Generated.FontFace.Core
import GHC.Wasm.Web.Generated.FontFaceDescriptors.Core
import GHC.Wasm.Web.Generated.FontFaceLoadStatus.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new FontFace($1,$2,$3)" js_cons_FontFace
  :: DOMString
     -> (JSObject (UnionClass '[DOMStringClass, BinaryDataClass])
         -> (Nullable FontFaceDescriptorsClass -> (IO FontFace)))
foreign import javascript safe "$1.load()" js_fun_load__Promise_FontFace
  :: FontFace -> (IO (Promise FontFaceClass))
foreign import javascript unsafe "$1.family" js_get_family
  :: FontFace -> (IO DOMString)
foreign import javascript unsafe "$1.family = $2" js_set_family
  :: FontFace -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.style" js_get_style
  :: FontFace -> (IO DOMString)
foreign import javascript unsafe "$1.style = $2" js_set_style
  :: FontFace -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.weight" js_get_weight
  :: FontFace -> (IO DOMString)
foreign import javascript unsafe "$1.weight = $2" js_set_weight
  :: FontFace -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.stretch" js_get_stretch
  :: FontFace -> (IO DOMString)
foreign import javascript unsafe "$1.stretch = $2" js_set_stretch
  :: FontFace -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.unicodeRange" js_get_unicodeRange
  :: FontFace -> (IO DOMString)
foreign import javascript unsafe "$1.unicodeRange = $2" js_set_unicodeRange
  :: FontFace -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.variant" js_get_variant
  :: FontFace -> (IO DOMString)
foreign import javascript unsafe "$1.variant = $2" js_set_variant
  :: FontFace -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.featureSettings" js_get_featureSettings
  :: FontFace -> (IO DOMString)
foreign import javascript unsafe "$1.featureSettings = $2" js_set_featureSettings
  :: FontFace -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.variationSettings" js_get_variationSettings
  :: FontFace -> (IO DOMString)
foreign import javascript unsafe "$1.variationSettings = $2" js_set_variationSettings
  :: FontFace -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.display" js_get_display
  :: FontFace -> (IO DOMString)
foreign import javascript unsafe "$1.display = $2" js_set_display
  :: FontFace -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.status" js_get_status
  :: FontFace -> (IO FontFaceLoadStatus)
foreign import javascript unsafe "$1.loaded" js_get_loaded
  :: FontFace -> (IO (Promise FontFaceClass))
