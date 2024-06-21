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
module GHC.Wasm.Web.Generated.DOMRect (
        DOMRect, DOMRectClass, js_cons_DOMRect, js_get_x, js_set_x,
        js_get_y, js_set_y, js_get_width, js_set_width, js_get_height,
        js_set_height
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMRect.Core
import GHC.Wasm.Web.Generated.DOMRectReadOnly.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new DOMRect($1,$2,$3,$4)" js_cons_DOMRect
  :: Nullable (JSPrimClass Double)
     -> (Nullable (JSPrimClass Double)
         -> (Nullable (JSPrimClass Double)
             -> (Nullable (JSPrimClass Double) -> (IO DOMRect))))
foreign import javascript unsafe "$1.x" js_get_x
  :: DOMRect -> (IO Double)
foreign import javascript unsafe "$1.x = $2" js_set_x
  :: DOMRect -> (Double -> (IO ()))
foreign import javascript unsafe "$1.y" js_get_y
  :: DOMRect -> (IO Double)
foreign import javascript unsafe "$1.y = $2" js_set_y
  :: DOMRect -> (Double -> (IO ()))
foreign import javascript unsafe "$1.width" js_get_width
  :: DOMRect -> (IO Double)
foreign import javascript unsafe "$1.width = $2" js_set_width
  :: DOMRect -> (Double -> (IO ()))
foreign import javascript unsafe "$1.height" js_get_height
  :: DOMRect -> (IO Double)
foreign import javascript unsafe "$1.height = $2" js_set_height
  :: DOMRect -> (Double -> (IO ()))
