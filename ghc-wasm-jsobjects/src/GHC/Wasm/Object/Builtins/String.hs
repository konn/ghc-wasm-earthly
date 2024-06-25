{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Builtins.String (
  -- * 'DOMString's
  DOMString,
  DOMStringClass,
  toDOMString,
  fromDOMString,

  -- * ByteStrings
  JSByteStringClass,
  JSByteString,
  toJSByteString,
  fromJSByteString,

  -- ** Conversion between WebIDL 'JSByteString' and Haskell 'BS.ByteString'
  toHaskellByteString,
  fromHaskellByteString,

  -- * 'USVString's
  USVStringClass,
  USVString,
  toUSVString,
  fromUSVString,

  -- * Conversion functions
  IsJavaScriptString (..),
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Coerce (coerce)
import Foreign
import Foreign.C (CChar)
import GHC.Exts (UnliftedType)
import GHC.Wasm.Object.Core
import GHC.Wasm.Prim

-- | A WebIDL @DOMString@ class, which corresponds to a JavaScript string.
type data DOMStringClass :: UnliftedType

type instance SuperclassOf DOMStringClass = 'Nothing

fromDOMString :: JSObject DOMStringClass -> JSString
fromDOMString = coerce . unJSObject

-- | A WebIDL @DOMString@ value, which corresponds to a JavaScript string.
type DOMString = JSObject DOMStringClass

foreign import javascript unsafe "if ($1 === null) { return \"\"; } else { return $1; }"
  js_null_to_empty :: JSString -> JSString

toDOMString ::
  -- | True if convert @null@ to empty string
  Bool ->
  JSString ->
  DOMString
toDOMString False = unsafeAsObject . coerce
toDOMString True = unsafeAsObject . coerce . js_null_to_empty

-- | A WebIDL @ByteString@ class, which corresponds to a JavaScript byte sequence as a string.
type data JSByteStringClass :: UnliftedType

type instance SuperclassOf JSByteStringClass = 'Nothing

type JSByteString = JSObject JSByteStringClass

fromJSByteString :: JSByteString -> JSString
fromJSByteString = JSString . unJSObject

toJSByteString :: JSString -> Maybe JSByteString
toJSByteString jsstr
  | js_is_bytestring jsstr = Just $ unsafeAsObject $ coerce jsstr
  | otherwise = Nothing

foreign import javascript unsafe "for (var i = 0; i < $1.length; i++) { if ($1.charCodeAt(i) > 255) { return false; } }  return true;"
  js_is_bytestring :: JSString -> Bool

foreign import javascript unsafe "$1.length"
  js_stringLength :: JSString -> IO Int

foreign import javascript unsafe "(new TextEncoder()).encodeInto($1, new Uint8Array(__exports.memory.buffer, $2, $3)).written"
  js_encodeInto :: JSString -> Ptr Word8 -> Int -> IO Int

toHaskellByteString :: JSByteString -> IO BS.ByteString
toHaskellByteString jsbs = do
  let jsstr = fromJSByteString jsbs
  len <- js_stringLength jsstr
  let lenMax = len * 3
  BS.createUptoN lenMax \buf -> js_encodeInto jsstr buf lenMax

foreign import javascript unsafe "(new TextDecoder('utf-8', {fatal: true})).decode(new Uint8Array(__exports.memory.buffer, $1, $2))"
  js_toJSString :: Ptr CChar -> Int -> IO JSByteString

fromHaskellByteString :: BS.ByteString -> IO JSByteString
fromHaskellByteString bs =
  BS.unsafeUseAsCStringLen bs $ uncurry js_toJSString

type data USVStringClass :: UnliftedType

type instance SuperclassOf USVStringClass = 'Nothing

type USVString = JSObject USVStringClass

fromUSVString :: USVString -> JSString
fromUSVString = coerce . unJSObject

toUSVString :: JSString -> USVString
toUSVString = unsafeAsObject . coerce

class IsJavaScriptString p where
  convertToJSString :: JSObject p -> JSString
  convertFromJSString :: JSString -> Maybe (JSObject p)

instance IsJavaScriptString DOMStringClass where
  convertFromJSString = Just . toDOMString False
  convertToJSString = fromDOMString

instance IsJavaScriptString USVStringClass where
  convertFromJSString = Just . toUSVString
  convertToJSString = fromUSVString

instance IsJavaScriptString JSByteStringClass where
  convertFromJSString = toJSByteString
  convertToJSString = fromJSByteString
