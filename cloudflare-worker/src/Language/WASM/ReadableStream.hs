{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.WASM.ReadableStream (
  ReadableStream (..),
  ReadableStreamDefaultReader (..),
  pop,
  getReader,
  toByteStream,
) where

import Control.Monad
import Control.Monad.Trans.Class (lift)
import Data.ByteString qualified as BS
import Foreign
import Foreign.C
import GHC.Wasm.Prim (JSVal)
import Streaming.ByteString qualified as Q

newtype ReadableStream = ReadableStream {runReadableStream :: JSVal}

newtype ReadableStreamDefaultReader = ReadableStreamDefaultReader {runReadableStreamDefaultReader :: JSVal}

newtype ReadableStreamResult = ReadableStreamResult JSVal

getReader :: ReadableStream -> IO ReadableStreamDefaultReader
getReader = js_rs_getReader

foreign import javascript unsafe "$1.getReader()"
  js_rs_getReader :: ReadableStream -> IO ReadableStreamDefaultReader

foreign import javascript "await $1.read()"
  js_rs_read :: ReadableStreamDefaultReader -> IO ReadableStreamResult

foreign import javascript unsafe "$1.done"
  js_stream_done :: ReadableStreamResult -> Bool

newtype Uint8Array = Uint8Array {runUint8Array :: Ptr CChar}

foreign import javascript unsafe "$1.value"
  js_stream_value :: ReadableStreamResult -> IO Uint8Array

foreign import javascript unsafe "$1.length"
  js_uint8_length :: Uint8Array -> IO Int

pop :: ReadableStreamDefaultReader -> IO (Maybe BS.ByteString)
pop reader = do
  result <- js_rs_read reader
  if js_stream_done result
    then pure Nothing
    else do
      vec <- js_stream_value result
      len <- js_uint8_length vec
      Just <$> BS.packCStringLen (runUint8Array vec, len)

toByteStream :: ReadableStream -> Q.ByteStream IO ()
toByteStream = Q.reread pop <=< lift . getReader
