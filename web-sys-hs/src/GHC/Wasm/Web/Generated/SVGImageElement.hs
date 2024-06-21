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
module GHC.Wasm.Web.Generated.SVGImageElement (
        SVGImageElement, SVGImageElementClass, js_get_x, js_get_y,
        js_get_width, js_get_height, js_get_preserveAspectRatio,
        js_get_href
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.SVGAnimatedLength.Core
import GHC.Wasm.Web.Generated.SVGAnimatedPreserveAspectRatio.Core
import GHC.Wasm.Web.Generated.SVGAnimatedString.Core
import GHC.Wasm.Web.Generated.SVGGraphicsElement.Core
import GHC.Wasm.Web.Generated.SVGImageElement.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.x" js_get_x
  :: SVGImageElement -> (IO SVGAnimatedLength)
foreign import javascript unsafe "$1.y" js_get_y
  :: SVGImageElement -> (IO SVGAnimatedLength)
foreign import javascript unsafe "$1.width" js_get_width
  :: SVGImageElement -> (IO SVGAnimatedLength)
foreign import javascript unsafe "$1.height" js_get_height
  :: SVGImageElement -> (IO SVGAnimatedLength)
foreign import javascript unsafe "$1.preserveAspectRatio" js_get_preserveAspectRatio
  :: SVGImageElement -> (IO SVGAnimatedPreserveAspectRatio)
foreign import javascript unsafe "$1.href" js_get_href
  :: SVGImageElement -> (IO SVGAnimatedString)
