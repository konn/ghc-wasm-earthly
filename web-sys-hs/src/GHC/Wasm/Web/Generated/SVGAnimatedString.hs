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
module GHC.Wasm.Web.Generated.SVGAnimatedString (
        SVGAnimatedString, SVGAnimatedStringClass, js_get_baseVal,
        js_set_baseVal, js_get_animVal
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.SVGAnimatedString.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.baseVal" js_get_baseVal
  :: SVGAnimatedString -> (IO DOMString)
foreign import javascript unsafe "$1.baseVal = $2" js_set_baseVal
  :: SVGAnimatedString -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.animVal" js_get_animVal
  :: SVGAnimatedString -> (IO DOMString)
