{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.WASM.ByteString (toByteString, fromByteString) where

import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Foreign
import GHC.Wasm.Prim

-- Conversion b/w JSString and ByteString.
-- Stolen from jsaddle-wasm: https://github.com/amesgen/jsaddle-wasm/blob/main/src/Language/Javascript/JSaddle/Wasm.hs

foreign import javascript unsafe "$1.length"
  js_stringLength :: JSString -> IO Int

foreign import javascript unsafe "(new TextEncoder()).encodeInto($1, new Uint8Array(__exports.memory.buffer, $2, $3)).written"
  js_encodeInto :: JSString -> Ptr a -> Int -> IO Int

toByteString :: JSString -> IO BS.ByteString
toByteString s = do
  len <- js_stringLength s
  -- see https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder/encodeInto#buffer_sizing
  -- (could also use another strategy described there)
  let lenMax = len * 3
  BS.createUptoN lenMax \buf -> js_encodeInto s buf lenMax

foreign import javascript unsafe "(new TextDecoder('utf-8', {fatal: true})).decode(new Uint8Array(__exports.memory.buffer, $1, $2))"
  js_toJSString :: Ptr a -> Int -> IO JSString

fromByteString :: BS.ByteString -> IO JSString
fromByteString bs =
  BS.unsafeUseAsCStringLen bs $ uncurry js_toJSString
