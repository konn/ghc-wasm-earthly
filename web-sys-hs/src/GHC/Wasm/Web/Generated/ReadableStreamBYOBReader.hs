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
module GHC.Wasm.Web.Generated.ReadableStreamBYOBReader (
        ReadableStreamBYOBReader, ReadableStreamBYOBReaderClass,
        js_cons_ReadableStreamBYOBReader,
        js_fun_read_ArrayBufferView_Promise_ReadableStreamReadResult,
        js_fun_releaseLock__undefined,
        js_fun_cancel_nullable_any_Promise_undefined, js_get_closed
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ReadableStream.Core
import GHC.Wasm.Web.Generated.ReadableStreamBYOBReader.Core
import GHC.Wasm.Web.Generated.ReadableStreamReadResult.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new ReadableStreamBYOBReader($1)" js_cons_ReadableStreamBYOBReader
  :: ReadableStream -> (IO ReadableStreamBYOBReader)
foreign import javascript safe "$1.read($2)" js_fun_read_ArrayBufferView_Promise_ReadableStreamReadResult
  :: ReadableStreamBYOBReader
     -> (ArrayBufferView
         -> (IO (Promise ReadableStreamReadResultClass)))
foreign import javascript unsafe "$1.releaseLock()" js_fun_releaseLock__undefined
  :: ReadableStreamBYOBReader -> (IO ())
foreign import javascript safe "$1.cancel($2)" js_fun_cancel_nullable_any_Promise_undefined
  :: ReadableStreamBYOBReader
     -> (Nullable AnyClass -> (IO (Promise UndefinedClass)))
foreign import javascript unsafe "$1.closed" js_get_closed
  :: ReadableStreamBYOBReader -> (IO (Promise UndefinedClass))
