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
module GHC.Wasm.Web.Generated.OffscreenCanvas (
        OffscreenCanvas, OffscreenCanvasClass, js_cons_OffscreenCanvas,
        js_fun_getContext_DOMString_nullable_any_nullable_nsISupports,
        js_fun_transferToImageBitmap__ImageBitmap,
        js_fun_convertToBlob_nullable_ImageEncodeOptions_Promise_Blob,
        js_get_width, js_set_width, js_get_height, js_set_height
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Blob.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.ImageBitmap.Core
import GHC.Wasm.Web.Generated.ImageEncodeOptions.Core
import GHC.Wasm.Web.Generated.NsISupports.Core
import GHC.Wasm.Web.Generated.OffscreenCanvas.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new OffscreenCanvas($1,$2)" js_cons_OffscreenCanvas
  :: Word32 -> (Word32 -> (IO OffscreenCanvas))
foreign import javascript unsafe "$1.getContext($2,$3)" js_fun_getContext_DOMString_nullable_any_nullable_nsISupports
  :: OffscreenCanvas
     -> (DOMString
         -> (Nullable AnyClass -> (IO (Nullable NsISupportsClass))))
foreign import javascript unsafe "$1.transferToImageBitmap()" js_fun_transferToImageBitmap__ImageBitmap
  :: OffscreenCanvas -> (IO ImageBitmap)
foreign import javascript safe "$1.convertToBlob($2)" js_fun_convertToBlob_nullable_ImageEncodeOptions_Promise_Blob
  :: OffscreenCanvas
     -> (Nullable ImageEncodeOptionsClass -> (IO (Promise BlobClass)))
foreign import javascript unsafe "$1.width" js_get_width
  :: OffscreenCanvas -> (IO Word32)
foreign import javascript unsafe "$1.width = $2" js_set_width
  :: OffscreenCanvas -> (Word32 -> (IO ()))
foreign import javascript unsafe "$1.height" js_get_height
  :: OffscreenCanvas -> (IO Word32)
foreign import javascript unsafe "$1.height = $2" js_set_height
  :: OffscreenCanvas -> (Word32 -> (IO ()))
