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
module GHC.Wasm.Web.Generated.ImageData (
        ImageData, ImageDataClass, js_cons_ImageData_long_long,
        js_cons_ImageData_Uint8ClampedArray_long_nullable_long,
        js_get_width, js_get_height, js_get_data
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ImageData.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new ImageData($1,$2)" js_cons_ImageData_long_long
  :: Word32 -> (Word32 -> (IO ImageData))
foreign import javascript unsafe "new ImageData($1,$2,$3)" js_cons_ImageData_Uint8ClampedArray_long_nullable_long
  :: JSByteArray Word8
     -> (Word32 -> (Nullable (JSPrimClass Word32) -> (IO ImageData)))
foreign import javascript unsafe "$1.width" js_get_width
  :: ImageData -> (IO Word32)
foreign import javascript unsafe "$1.height" js_get_height
  :: ImageData -> (IO Word32)
foreign import javascript unsafe "$1.data" js_get_data
  :: ImageData -> (IO (JSByteArray Word8))
