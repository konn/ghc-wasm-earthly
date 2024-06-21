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
module GHC.Wasm.Web.Generated.SVGPoint (
        SVGPoint, SVGPointClass, js_fun_matrixTransform_SVGMatrix_SVGPoint,
        js_get_x, js_set_x, js_get_y, js_set_y
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.SVGMatrix.Core
import GHC.Wasm.Web.Generated.SVGPoint.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.matrixTransform($2)" js_fun_matrixTransform_SVGMatrix_SVGPoint
  :: SVGPoint -> (SVGMatrix -> (IO SVGPoint))
foreign import javascript unsafe "$1.x" js_get_x
  :: SVGPoint -> (IO Float)
foreign import javascript unsafe "$1.x = $2" js_set_x
  :: SVGPoint -> (Float -> (IO ()))
foreign import javascript unsafe "$1.y" js_get_y
  :: SVGPoint -> (IO Float)
foreign import javascript unsafe "$1.y = $2" js_set_y
  :: SVGPoint -> (Float -> (IO ()))
