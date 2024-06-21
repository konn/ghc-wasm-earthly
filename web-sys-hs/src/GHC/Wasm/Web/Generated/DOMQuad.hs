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
module GHC.Wasm.Web.Generated.DOMQuad (
        DOMQuad, DOMQuadClass,
        js_cons_DOMQuad_nullable_DOMPointInit_nullable_DOMPointInit_nullable_DOMPointInit_nullable_DOMPointInit,
        js_cons_DOMQuad_DOMRectReadOnly, js_fun_getBounds__DOMRectReadOnly,
        js_fun_toJSON__DOMQuadJSON, js_get_p1, js_get_p2, js_get_p3,
        js_get_p4, js_get_bounds
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMPoint.Core
import GHC.Wasm.Web.Generated.DOMPointInit.Core
import GHC.Wasm.Web.Generated.DOMQuad.Core
import GHC.Wasm.Web.Generated.DOMQuadJSON.Core
import GHC.Wasm.Web.Generated.DOMRectReadOnly.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new DOMQuad($1,$2,$3,$4)" js_cons_DOMQuad_nullable_DOMPointInit_nullable_DOMPointInit_nullable_DOMPointInit_nullable_DOMPointInit
  :: Nullable DOMPointInitClass
     -> (Nullable DOMPointInitClass
         -> (Nullable DOMPointInitClass
             -> (Nullable DOMPointInitClass -> (IO DOMQuad))))
foreign import javascript unsafe "new DOMQuad($1)" js_cons_DOMQuad_DOMRectReadOnly
  :: DOMRectReadOnly -> (IO DOMQuad)
foreign import javascript unsafe "$1.getBounds()" js_fun_getBounds__DOMRectReadOnly
  :: DOMQuad -> (IO DOMRectReadOnly)
foreign import javascript unsafe "$1.toJSON()" js_fun_toJSON__DOMQuadJSON
  :: DOMQuad -> (IO DOMQuadJSON)
foreign import javascript unsafe "$1.p1" js_get_p1
  :: DOMQuad -> (IO DOMPoint)
foreign import javascript unsafe "$1.p2" js_get_p2
  :: DOMQuad -> (IO DOMPoint)
foreign import javascript unsafe "$1.p3" js_get_p3
  :: DOMQuad -> (IO DOMPoint)
foreign import javascript unsafe "$1.p4" js_get_p4
  :: DOMQuad -> (IO DOMPoint)
foreign import javascript unsafe "$1.bounds" js_get_bounds
  :: DOMQuad -> (IO DOMRectReadOnly)
