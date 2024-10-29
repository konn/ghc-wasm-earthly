module GHC.Wasm.Web.JSON (
  encodeJSON,
  decodeJSON,
  eitherDecodeJSON,
  module GHC.Wasm.Web.Generated.JSON,
) where

import Control.Monad ((<=<))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as LBS
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.JSON

-- | NOTE: This converts a value with @JSON.parse@ so all reference to JSVal is lost.
encodeJSON :: (ToJSON a) => a -> IO JSON
encodeJSON = js_parse_json <=< fromHaskellByteString . LBS.toStrict . J.encode

foreign import javascript unsafe "JSON.parse($1)"
  js_parse_json :: JSByteString -> IO JSON

foreign import javascript unsafe "JSON.stringify($1)"
  js_stringify_json :: JSON -> IO USVString

-- | NOTE: This converts a value with @JSON.stringify@ so all reference to JSVal is lost and may be expensive when the object is large.
decodeJSON :: (FromJSON a) => JSON -> IO (Maybe a)
decodeJSON =
  fmap (J.decodeStrictText . toText) . js_stringify_json

-- | NOTE: This converts a value with @JSON.stringify@ so all reference to JSVal is lost and may be expensive when the object is large.
eitherDecodeJSON :: (FromJSON a) => JSON -> IO (Either String a)
eitherDecodeJSON =
  fmap (J.eitherDecodeStrictText . toText) . js_stringify_json
