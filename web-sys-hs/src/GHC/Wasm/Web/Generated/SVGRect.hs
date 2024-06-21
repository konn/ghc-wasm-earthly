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
module GHC.Wasm.Web.Generated.SVGRect (
        SVGRect, SVGRectClass, js_get_x, js_set_x, js_get_y, js_set_y,
        js_get_width, js_set_width, js_get_height, js_set_height
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.SVGRect.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.x" js_get_x
  :: SVGRect -> (IO Float)
foreign import javascript unsafe "$1.x = $2" js_set_x
  :: SVGRect -> (Float -> (IO ()))
foreign import javascript unsafe "$1.y" js_get_y
  :: SVGRect -> (IO Float)
foreign import javascript unsafe "$1.y = $2" js_set_y
  :: SVGRect -> (Float -> (IO ()))
foreign import javascript unsafe "$1.width" js_get_width
  :: SVGRect -> (IO Float)
foreign import javascript unsafe "$1.width = $2" js_set_width
  :: SVGRect -> (Float -> (IO ()))
foreign import javascript unsafe "$1.height" js_get_height
  :: SVGRect -> (IO Float)
foreign import javascript unsafe "$1.height = $2" js_set_height
  :: SVGRect -> (Float -> (IO ()))
