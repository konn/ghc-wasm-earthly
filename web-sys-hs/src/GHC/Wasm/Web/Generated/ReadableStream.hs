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
module GHC.Wasm.Web.Generated.ReadableStream (
        ReadableStream, ReadableStreamClass, js_cons_ReadableStream,
        js_fun_cancel_nullable_any_Promise_undefined,
        js_fun_getReader_nullable_ReadableStreamGetReaderOptions_ReadableStreamReader,
        js_fun_pipeThrough_ReadableWritablePair_nullable_StreamPipeOptions_ReadableStream,
        js_fun_pipeTo_WritableStream_nullable_StreamPipeOptions_Promise_undefined,
        js_fun_tee__sequence_ReadableStream, js_get_locked
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.QueuingStrategy.Core
import GHC.Wasm.Web.Generated.ReadableStream.Core
import GHC.Wasm.Web.Generated.ReadableStreamGetReaderOptions.Core
import GHC.Wasm.Web.Generated.ReadableStreamIteratorOptions.Core
import GHC.Wasm.Web.Generated.ReadableStreamReader.Core
import GHC.Wasm.Web.Generated.ReadableWritablePair.Core
import GHC.Wasm.Web.Generated.StreamPipeOptions.Core
import GHC.Wasm.Web.Generated.WritableStream.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new ReadableStream($1,$2)" js_cons_ReadableStream
  :: Nullable AnyClass
     -> (Nullable QueuingStrategyClass -> (IO ReadableStream))
foreign import javascript safe "$1.cancel($2)" js_fun_cancel_nullable_any_Promise_undefined
  :: ReadableStream
     -> (Nullable AnyClass -> (IO (Promise UndefinedClass)))
foreign import javascript unsafe "$1.getReader($2)" js_fun_getReader_nullable_ReadableStreamGetReaderOptions_ReadableStreamReader
  :: ReadableStream
     -> (Nullable ReadableStreamGetReaderOptionsClass
         -> (IO ReadableStreamReader))
foreign import javascript unsafe "$1.pipeThrough($2,$3)" js_fun_pipeThrough_ReadableWritablePair_nullable_StreamPipeOptions_ReadableStream
  :: ReadableStream
     -> (ReadableWritablePair
         -> (Nullable StreamPipeOptionsClass -> (IO ReadableStream)))
foreign import javascript safe "$1.pipeTo($2,$3)" js_fun_pipeTo_WritableStream_nullable_StreamPipeOptions_Promise_undefined
  :: ReadableStream
     -> (WritableStream
         -> (Nullable StreamPipeOptionsClass
             -> (IO (Promise UndefinedClass))))
foreign import javascript unsafe "$1.tee()" js_fun_tee__sequence_ReadableStream
  :: ReadableStream -> (IO (Sequence ReadableStreamClass))
foreign import javascript unsafe "$1.locked" js_get_locked
  :: ReadableStream -> (IO Bool)
