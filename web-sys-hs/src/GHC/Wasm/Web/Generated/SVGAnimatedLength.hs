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
module GHC.Wasm.Web.Generated.SVGAnimatedLength (
        SVGAnimatedLength, SVGAnimatedLengthClass, js_get_baseVal,
        js_get_animVal
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.SVGAnimatedLength.Core
import GHC.Wasm.Web.Generated.SVGLength.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.baseVal" js_get_baseVal
  :: SVGAnimatedLength -> (IO SVGLength)
foreign import javascript unsafe "$1.animVal" js_get_animVal
  :: SVGAnimatedLength -> (IO SVGLength)
