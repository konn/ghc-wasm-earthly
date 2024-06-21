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
module GHC.Wasm.Web.Generated.Blob (
        Blob, BlobClass, js_cons_Blob,
        js_fun_slice_nullable_longlong_nullable_longlong_nullable_DOMString_Blob,
        js_fun_stream__ReadableStream, js_fun_text__Promise_DOMString,
        js_fun_arrayBuffer__Promise_ArrayBuffer, js_get_size, js_get_type
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Blob.Core
import GHC.Wasm.Web.Generated.BlobPart.Core
import GHC.Wasm.Web.Generated.BlobPropertyBag.Core
import GHC.Wasm.Web.Generated.ReadableStream.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new Blob($1,$2)" js_cons_Blob
  :: Nullable (SequenceClass BlobPartClass)
     -> (Nullable BlobPropertyBagClass -> (IO Blob))
foreign import javascript unsafe "$1.slice($2,$3,$4)" js_fun_slice_nullable_longlong_nullable_longlong_nullable_DOMString_Blob
  :: Blob
     -> (Nullable (JSPrimClass Int64)
         -> (Nullable (JSPrimClass Int64)
             -> (Nullable DOMStringClass -> (IO Blob))))
foreign import javascript unsafe "$1.stream()" js_fun_stream__ReadableStream
  :: Blob -> (IO ReadableStream)
foreign import javascript safe "$1.text()" js_fun_text__Promise_DOMString
  :: Blob -> (IO (Promise DOMStringClass))
foreign import javascript safe "$1.arrayBuffer()" js_fun_arrayBuffer__Promise_ArrayBuffer
  :: Blob -> (IO (Promise ArrayBufferClass))
foreign import javascript unsafe "$1.size" js_get_size
  :: Blob -> (IO Word64)
foreign import javascript unsafe "$1.type" js_get_type
  :: Blob -> (IO DOMString)
