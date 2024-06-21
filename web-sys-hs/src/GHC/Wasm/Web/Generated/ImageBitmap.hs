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
module GHC.Wasm.Web.Generated.ImageBitmap (
        ImageBitmap, ImageBitmapClass, js_fun_close__undefined,
        js_get_width, js_get_height
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ImageBitmap.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.close()" js_fun_close__undefined
  :: ImageBitmap -> (IO ())
foreign import javascript unsafe "$1.width" js_get_width
  :: ImageBitmap -> (IO Word32)
foreign import javascript unsafe "$1.height" js_get_height
  :: ImageBitmap -> (IO Word32)
