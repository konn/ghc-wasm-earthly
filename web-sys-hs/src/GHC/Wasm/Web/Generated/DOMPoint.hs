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
module GHC.Wasm.Web.Generated.DOMPoint (
        DOMPoint, DOMPointClass, js_cons_DOMPoint, js_get_x, js_set_x,
        js_get_y, js_set_y, js_get_z, js_set_z, js_get_w, js_set_w
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMPoint.Core
import GHC.Wasm.Web.Generated.DOMPointInit.Core
import GHC.Wasm.Web.Generated.DOMPointReadOnly.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new DOMPoint($1,$2,$3,$4)" js_cons_DOMPoint
  :: Nullable (JSPrimClass Double)
     -> (Nullable (JSPrimClass Double)
         -> (Nullable (JSPrimClass Double)
             -> (Nullable (JSPrimClass Double) -> (IO DOMPoint))))
foreign import javascript unsafe "$1.x" js_get_x
  :: DOMPoint -> (IO Double)
foreign import javascript unsafe "$1.x = $2" js_set_x
  :: DOMPoint -> (Double -> (IO ()))
foreign import javascript unsafe "$1.y" js_get_y
  :: DOMPoint -> (IO Double)
foreign import javascript unsafe "$1.y = $2" js_set_y
  :: DOMPoint -> (Double -> (IO ()))
foreign import javascript unsafe "$1.z" js_get_z
  :: DOMPoint -> (IO Double)
foreign import javascript unsafe "$1.z = $2" js_set_z
  :: DOMPoint -> (Double -> (IO ()))
foreign import javascript unsafe "$1.w" js_get_w
  :: DOMPoint -> (IO Double)
foreign import javascript unsafe "$1.w = $2" js_set_w
  :: DOMPoint -> (Double -> (IO ()))
