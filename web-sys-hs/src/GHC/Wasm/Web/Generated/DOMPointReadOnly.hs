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
module GHC.Wasm.Web.Generated.DOMPointReadOnly (
        DOMPointReadOnly, DOMPointReadOnlyClass, js_cons_DOMPointReadOnly,
        js_fun_toJSON__object, js_get_x, js_get_y, js_get_z, js_get_w
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMPointInit.Core
import GHC.Wasm.Web.Generated.DOMPointReadOnly.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new DOMPointReadOnly($1,$2,$3,$4)" js_cons_DOMPointReadOnly
  :: Nullable (JSPrimClass Double)
     -> (Nullable (JSPrimClass Double)
         -> (Nullable (JSPrimClass Double)
             -> (Nullable (JSPrimClass Double) -> (IO DOMPointReadOnly))))
foreign import javascript unsafe "$1.toJSON()" js_fun_toJSON__object
  :: DOMPointReadOnly -> (IO JSAny)
foreign import javascript unsafe "$1.x" js_get_x
  :: DOMPointReadOnly -> (IO Double)
foreign import javascript unsafe "$1.y" js_get_y
  :: DOMPointReadOnly -> (IO Double)
foreign import javascript unsafe "$1.z" js_get_z
  :: DOMPointReadOnly -> (IO Double)
foreign import javascript unsafe "$1.w" js_get_w
  :: DOMPointReadOnly -> (IO Double)
