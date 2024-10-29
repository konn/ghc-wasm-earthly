{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE UnliftedDatatypes #-}

module GHC.Wasm.Web.JSON (
  encodeJSON,
  decodeJSON,
  eitherDecodeJSON,
  stringify,
  parse,
  module GHC.Wasm.Web.Generated.JSON,

  -- ** Experimental
  parseJSONFromJS,
  valueToJSON,
) where

import Control.Exception (evaluate)
import Control.Monad ((<=<))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as F
import Data.Functor (void)
import Data.Scientific (floatingOrInteger)
import GHC.Exts (noinline)
import GHC.Wasm.Object.Builtins hiding (parse)
import qualified GHC.Wasm.Object.Builtins.Sequence as Seq
import GHC.Wasm.Prim
import GHC.Wasm.Web.Generated.JSON
import System.IO.Unsafe (unsafePerformIO)

-- | NOTE: This converts a value with @JSON.parse@ so all reference to JSVal is lost.
encodeJSON :: (ToJSON a) => a -> IO JSON
encodeJSON = pure . valueToJSON . J.toJSON

stringify :: JSON -> IO USVString
stringify = js_stringify_json

parse :: USVString -> IO JSON
parse = js_parse_json

foreign import javascript unsafe "JSON.parse($1)"
  js_parse_json_bs :: JSByteString -> IO JSON

foreign import javascript unsafe "JSON.parse($1)"
  js_parse_json :: USVString -> IO JSON

foreign import javascript unsafe "JSON.stringify($1)"
  js_stringify_json :: JSON -> IO USVString

-- | NOTE: This converts a value with @JSON.stringify@ so all reference to JSVal is lost and may be expensive when the object is large.
decodeJSON :: (FromJSON a) => JSON -> IO (Maybe a)
decodeJSON =
  pure . (maybeResult . J.fromJSON <=< either (const Nothing) Just . parseJSONFromJS)

maybeResult :: J.Result a -> Maybe a
{-# INLINE maybeResult #-}
maybeResult = \case
  J.Success a -> Just a
  J.Error _ -> Nothing

-- | NOTE: This converts a value with @JSON.stringify@ so all reference to JSVal is lost and may be expensive when the object is large.
eitherDecodeJSON :: (FromJSON a) => JSON -> IO (Either String a)
eitherDecodeJSON =
  pure . (eitherResult . J.fromJSON <=< parseJSONFromJS)

eitherResult :: J.Result a -> Either String a
{-# INLINE eitherResult #-}
eitherResult = \case
  J.Success a -> Right a
  J.Error e -> Left e

type data JSONObjectClass :: Prototype

type JSONObject = JSObject JSONObjectClass

valueToJSON :: J.Value -> JSON
valueToJSON (J.String s) = unsafeCast $ fromText @USVStringClass s
valueToJSON (J.Number n) = case floatingOrInteger n of
  Left d -> unsafeCast $ toJSPrim @Double d
  Right i -> unsafeCast $ toJSPrim @Int i
valueToJSON (J.Bool b) = unsafeCast $ toJSPrim b
valueToJSON J.Null = unsafeCast $ none @JSONClass
valueToJSON (J.Array arr) = unsafeCast $ Seq.toSequence $ fmap valueToJSON arr
valueToJSON (J.Object obj) =
  noinline
    ( unsafePerformIO do
        dic <- js_new_obj
        void $
          AKM.traverseWithKey
            ( \k v -> do
                val <- evaluate $ valueToJSON v
                js_set_prop dic (fromText $ AK.toText k) val
            )
            obj
        pure dic
    )

parseJSONFromJS :: JSON -> Either String J.Value
parseJSONFromJS json
  | js_is_null json = pure J.Null
  | js_is_number json = do
      case fromNullable $ js_decode_int json of
        Just int' -> pure $ J.Number $ fromIntegral $ fromJSPrim int'
        Nothing -> do
          nullable
            (Left $ "JSON: number is neither integral nor double: " <> fromJSString (js_typeof json))
            (Right . J.Number . realToFrac . fromJSPrim)
            $ js_decode_double json
  | Just a <- fromNullable (js_decode_string json) = pure $ J.String $ toText a
  | Just b <- fromNullable $ js_decode_bool json = pure $ J.Bool $ fromJSPrim b
  | Just arr <- fromNullable $ js_decode_array json = do
      fmap J.Array $ mapM parseJSONFromJS $ unsafePerformIO $ Seq.toVector arr
  | Just obj <- fromNullable $ js_decode_object json = do
      let props = unsafePerformIO $ Seq.toVector $ js_props obj
      val <- mapM (\k -> (AK.fromText $ toText k,) <$> parseJSONFromJS (js_get_prop obj k)) props
      pure $ J.Object $ AKM.fromList $ F.toList val
  | otherwise = Left $ "Invalid JSON value: the value of type " <> fromJSString (js_typeof json) <> " given."

foreign import javascript unsafe "typeof $1"
  js_typeof :: JSON -> JSString

foreign import javascript unsafe "if (typeof $1 === 'object') { return $1 } else { return null}"
  js_decode_object :: JSON -> Nullable JSONObjectClass

foreign import javascript unsafe "Object.getOwnPropertyNames($1)"
  js_props :: JSONObject -> Sequence USVStringClass

foreign import javascript unsafe "Object.getOwnPropertyNames($1)"
  js_get_prop :: JSONObject -> USVString -> JSON

foreign import javascript unsafe "$1 === null || $1 === undefined"
  js_is_null :: JSON -> Bool

foreign import javascript unsafe "typeof $1 === 'number'"
  js_is_number :: JSON -> Bool

foreign import javascript unsafe "if (typeof $1 === 'boolean') { return $1 } else { return null }"
  js_decode_bool :: JSON -> Nullable (JSPrimClass Bool)

foreign import javascript unsafe "if (typeof $1 === 'number' && Number.isInteger($1)) { return $1; } else { return null; }"
  js_decode_int :: JSON -> Nullable (JSPrimClass Int)

foreign import javascript unsafe "if (typeof $1 === 'number') { if (Number.isInteger($1)) { return null } else { return $1; } } else { return null; }"
  js_decode_double :: JSON -> Nullable (JSPrimClass Double)

foreign import javascript unsafe "if (typeof $1 === 'string') { return $1; } else { return null; }"
  js_decode_string :: JSON -> Nullable USVStringClass

foreign import javascript unsafe "if (Array.isArray($1)) { return $1; } else { return null; }"
  js_decode_array :: JSON -> Nullable (SequenceClass JSONClass)

foreign import javascript unsafe "{}"
  js_new_obj :: IO JSON

foreign import javascript unsafe "$1[$2] = $3"
  js_set_prop :: JSON -> USVString -> JSON -> IO ()
