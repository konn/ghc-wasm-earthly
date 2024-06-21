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
module GHC.Wasm.Web.Generated.FontFaceSetForEachCallback.Core (
        FontFaceSetForEachCallbackClass, FontFaceSetForEachCallback,
        js_mk_callback_FontFaceSetForEachCallback_impure
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.FontFace.Core
import GHC.Wasm.Web.Generated.FontFaceSet.Core
import GHC.Wasm.Web.Types
type data FontFaceSetForEachCallbackClass :: Prototype
type instance SuperclassOf FontFaceSetForEachCallbackClass = 'Nothing
type FontFaceSetForEachCallback =
    JSObject FontFaceSetForEachCallbackClass
foreign import javascript unsafe "wrapper" js_mk_callback_FontFaceSetForEachCallback_impure
  :: (FontFace -> (FontFace -> (FontFaceSet -> (IO ()))))
     -> FontFaceSetForEachCallback
