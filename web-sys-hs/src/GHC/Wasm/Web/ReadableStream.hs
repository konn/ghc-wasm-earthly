{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}

module GHC.Wasm.Web.ReadableStream (
  toReadableStream,
  fromReadableStream,
  module GHC.Wasm.Web.Generated.ReadableStream,
) where

import Control.Exception (finally)
import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Data.Word
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ReadableStream
import GHC.Wasm.Web.Generated.ReadableStreamDefaultReader
import qualified Streaming.ByteString as Q

toReadableStream :: Q.ByteStream IO () -> IO ReadableStream
toReadableStream q = do
  cancel <- js_mk_CancelStream mempty
  start <- js_mk_StrartStream \ctrl ->
    Q.chunkMapM_ (flip useByteStringAsJSByteArray $ js_enq ctrl) q
      `finally` js_cls ctrl
  js_new_ReadableStream start cancel

type data StreamControllerClass :: Prototype

type instance SuperclassOf StreamControllerClass = 'Nothing

type StreamController = JSObject StreamControllerClass

type data JSStartStreamClass :: Prototype

type instance SuperclassOf JSStartStreamClass = 'Nothing

type JSStartStream = JSObject JSStartStreamClass

type data JSCancelStreamClass :: Prototype

type instance SuperclassOf JSCancelStreamClass = 'Nothing

type JSCancelStream = JSObject JSCancelStreamClass

type StartStream = StreamController -> IO ()

type CancelStream = USVString -> IO ()

foreign import javascript unsafe "$1.enqueue($2)"
  js_enq :: StreamController -> Uint8Array -> IO ()

foreign import javascript unsafe "$1.close()"
  js_cls :: StreamController -> IO ()

foreign import javascript unsafe "wrapper"
  js_mk_StrartStream :: StartStream -> IO JSStartStream

foreign import javascript unsafe "wrapper"
  js_mk_CancelStream :: CancelStream -> IO JSCancelStream

foreign import javascript unsafe "new ReadableStream({ start(ctrl) { $1(ctrl); }, cancel(msg) { $2(msg); }, type: 'bytes' })"
  js_new_ReadableStream :: JSStartStream -> JSCancelStream -> IO ReadableStream

fromReadableStream :: ReadableStream -> Q.ByteStream IO ()
fromReadableStream =
  fromReadResult
    <=< lift
      . ( fmap (unsafeCast @_ @(ReadableStreamDefaultReaderClass))
            . flip
              js_fun_getReader_nullable_ReadableStreamGetReaderOptions_ReadableStreamReader
              none
        )

fromReadResult :: ReadableStreamDefaultReader -> Q.ByteStream IO ()
fromReadResult = Q.reread \inp -> do
  resl <- await =<< js_fun_read__Promise_ReadableStreamReadResult inp
  done <- getDictField "done" resl
  value <-
    fmap (toByteString . unsafeCast @_ @(JSByteArrayClass Word8)) . fromNullable
      <$> getDictField "value" resl
  if maybe True fromJSPrim $ fromNullable done
    then pure Nothing
    else pure value
