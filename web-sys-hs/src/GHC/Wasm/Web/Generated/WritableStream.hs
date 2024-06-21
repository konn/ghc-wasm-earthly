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
module GHC.Wasm.Web.Generated.WritableStream (
        WritableStream, WritableStreamClass, js_cons_WritableStream,
        js_fun_abort_nullable_any_Promise_undefined,
        js_fun_close__Promise_undefined,
        js_fun_getWriter__WritableStreamDefaultWriter, js_get_locked
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.QueuingStrategy.Core
import GHC.Wasm.Web.Generated.WritableStream.Core
import GHC.Wasm.Web.Generated.WritableStreamDefaultWriter.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new WritableStream($1,$2)" js_cons_WritableStream
  :: Nullable AnyClass
     -> (Nullable QueuingStrategyClass -> (IO WritableStream))
foreign import javascript safe "$1.abort($2)" js_fun_abort_nullable_any_Promise_undefined
  :: WritableStream
     -> (Nullable AnyClass -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.close()" js_fun_close__Promise_undefined
  :: WritableStream -> (IO (Promise UndefinedClass))
foreign import javascript unsafe "$1.getWriter()" js_fun_getWriter__WritableStreamDefaultWriter
  :: WritableStream -> (IO WritableStreamDefaultWriter)
foreign import javascript unsafe "$1.locked" js_get_locked
  :: WritableStream -> (IO Bool)
