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
module GHC.Wasm.Web.Generated.WritableStreamDefaultWriter (
        WritableStreamDefaultWriter, WritableStreamDefaultWriterClass,
        js_cons_WritableStreamDefaultWriter,
        js_fun_abort_nullable_any_Promise_undefined,
        js_fun_close__Promise_undefined, js_fun_releaseLock__undefined,
        js_fun_write_nullable_any_Promise_undefined, js_get_closed,
        js_get_desiredSize, js_get_ready
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.WritableStream.Core
import GHC.Wasm.Web.Generated.WritableStreamDefaultWriter.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new WritableStreamDefaultWriter($1)" js_cons_WritableStreamDefaultWriter
  :: WritableStream -> (IO WritableStreamDefaultWriter)
foreign import javascript safe "$1.abort($2)" js_fun_abort_nullable_any_Promise_undefined
  :: WritableStreamDefaultWriter
     -> (Nullable AnyClass -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.close()" js_fun_close__Promise_undefined
  :: WritableStreamDefaultWriter -> (IO (Promise UndefinedClass))
foreign import javascript unsafe "$1.releaseLock()" js_fun_releaseLock__undefined
  :: WritableStreamDefaultWriter -> (IO ())
foreign import javascript safe "$1.write($2)" js_fun_write_nullable_any_Promise_undefined
  :: WritableStreamDefaultWriter
     -> (Nullable AnyClass -> (IO (Promise UndefinedClass)))
foreign import javascript unsafe "$1.closed" js_get_closed
  :: WritableStreamDefaultWriter -> (IO (Promise UndefinedClass))
foreign import javascript unsafe "$1.desiredSize" js_get_desiredSize
  :: WritableStreamDefaultWriter
     -> (IO (Nullable (JSPrimClass Double)))
foreign import javascript unsafe "$1.ready" js_get_ready
  :: WritableStreamDefaultWriter -> (IO (Promise UndefinedClass))
