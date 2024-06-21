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
module GHC.Wasm.Web.Generated.FontData (
        FontData, FontDataClass, js_fun_blob__Promise_Blob,
        js_get_postscriptName, js_get_fullName, js_get_family, js_get_style
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Blob.Core
import GHC.Wasm.Web.Generated.FontData.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.blob()" js_fun_blob__Promise_Blob
  :: FontData -> (IO (Promise BlobClass))
foreign import javascript unsafe "$1.postscriptName" js_get_postscriptName
  :: FontData -> (IO USVString)
foreign import javascript unsafe "$1.fullName" js_get_fullName
  :: FontData -> (IO USVString)
foreign import javascript unsafe "$1.family" js_get_family
  :: FontData -> (IO USVString)
foreign import javascript unsafe "$1.style" js_get_style
  :: FontData -> (IO USVString)
