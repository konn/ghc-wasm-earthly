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
module GHC.Wasm.Web.Generated.DOMRectReadOnly (
        DOMRectReadOnly, DOMRectReadOnlyClass, js_cons_DOMRectReadOnly,
        js_fun_toJSON__object, js_get_x, js_get_y, js_get_width,
        js_get_height, js_get_top, js_get_right, js_get_bottom, js_get_left
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMRectReadOnly.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new DOMRectReadOnly($1,$2,$3,$4)" js_cons_DOMRectReadOnly
  :: Nullable (JSPrimClass Double)
     -> (Nullable (JSPrimClass Double)
         -> (Nullable (JSPrimClass Double)
             -> (Nullable (JSPrimClass Double) -> (IO DOMRectReadOnly))))
foreign import javascript unsafe "$1.toJSON()" js_fun_toJSON__object
  :: DOMRectReadOnly -> (IO JSAny)
foreign import javascript unsafe "$1.x" js_get_x
  :: DOMRectReadOnly -> (IO Double)
foreign import javascript unsafe "$1.y" js_get_y
  :: DOMRectReadOnly -> (IO Double)
foreign import javascript unsafe "$1.width" js_get_width
  :: DOMRectReadOnly -> (IO Double)
foreign import javascript unsafe "$1.height" js_get_height
  :: DOMRectReadOnly -> (IO Double)
foreign import javascript unsafe "$1.top" js_get_top
  :: DOMRectReadOnly -> (IO Double)
foreign import javascript unsafe "$1.right" js_get_right
  :: DOMRectReadOnly -> (IO Double)
foreign import javascript unsafe "$1.bottom" js_get_bottom
  :: DOMRectReadOnly -> (IO Double)
foreign import javascript unsafe "$1.left" js_get_left
  :: DOMRectReadOnly -> (IO Double)
