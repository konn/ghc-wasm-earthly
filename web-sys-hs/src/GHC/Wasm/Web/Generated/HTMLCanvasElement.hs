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
module GHC.Wasm.Web.Generated.HTMLCanvasElement (
        HTMLCanvasElement, HTMLCanvasElementClass,
        js_fun_getContext_DOMString_nullable_any_nullable_nsISupports,
        js_fun_toDataURL_nullable_DOMString_nullable_any_DOMString,
        js_fun_toBlob_BlobCallback_nullable_DOMString_nullable_any_undefined,
        js_fun_transferControlToOffscreen__OffscreenCanvas,
        js_fun_captureStream_nullable_double_MediaStream, js_get_width,
        js_set_width, js_get_height, js_set_height
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.BlobCallback.Core
import GHC.Wasm.Web.Generated.HTMLCanvasElement.Core
import GHC.Wasm.Web.Generated.HTMLElement.Core
import GHC.Wasm.Web.Generated.MediaStream.Core
import GHC.Wasm.Web.Generated.NsISupports.Core
import GHC.Wasm.Web.Generated.OffscreenCanvas.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getContext($2,$3)" js_fun_getContext_DOMString_nullable_any_nullable_nsISupports
  :: HTMLCanvasElement
     -> (DOMString
         -> (Nullable AnyClass -> (IO (Nullable NsISupportsClass))))
foreign import javascript unsafe "$1.toDataURL($2,$3)" js_fun_toDataURL_nullable_DOMString_nullable_any_DOMString
  :: HTMLCanvasElement
     -> (Nullable DOMStringClass
         -> (Nullable AnyClass -> (IO DOMString)))
foreign import javascript unsafe "$1.toBlob($2,$3,$4)" js_fun_toBlob_BlobCallback_nullable_DOMString_nullable_any_undefined
  :: HTMLCanvasElement
     -> (BlobCallback
         -> (Nullable DOMStringClass -> (Nullable AnyClass -> (IO ()))))
foreign import javascript unsafe "$1.transferControlToOffscreen()" js_fun_transferControlToOffscreen__OffscreenCanvas
  :: HTMLCanvasElement -> (IO OffscreenCanvas)
foreign import javascript unsafe "$1.captureStream($2)" js_fun_captureStream_nullable_double_MediaStream
  :: HTMLCanvasElement
     -> (Nullable (JSPrimClass Double) -> (IO MediaStream))
foreign import javascript unsafe "$1.width" js_get_width
  :: HTMLCanvasElement -> (IO Word32)
foreign import javascript unsafe "$1.width = $2" js_set_width
  :: HTMLCanvasElement -> (Word32 -> (IO ()))
foreign import javascript unsafe "$1.height" js_get_height
  :: HTMLCanvasElement -> (IO Word32)
foreign import javascript unsafe "$1.height = $2" js_set_height
  :: HTMLCanvasElement -> (Word32 -> (IO ()))
