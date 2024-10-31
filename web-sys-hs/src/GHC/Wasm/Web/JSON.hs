{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE ViewPatterns #-}

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

import Control.Applicative (asum)
import Control.Exception (evaluate)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Bifunctor as Bi
import qualified Data.Foldable as F
import Data.Functor (void)
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Wasm.Object.Builtins hiding (parse)
import qualified GHC.Wasm.Object.Builtins.Sequence as Seq
import GHC.Wasm.Prim
import GHC.Wasm.Web.Generated.JSON

-- | NOTE: This converts a value with @JSON.parse@ so all reference to JSVal is lost.
encodeJSON :: (ToJSON a) => a -> IO JSON
encodeJSON = valueToJSON . J.toJSON

stringify :: JSON -> IO USVString
stringify = js_stringify_json

parse :: USVString -> IO JSON
parse = js_parse_json

foreign import javascript unsafe "JSON.parse($1)"
  js_parse_json :: USVString -> IO JSON

foreign import javascript unsafe "JSON.stringify($1)"
  js_stringify_json :: JSON -> IO USVString

-- | NOTE: This converts a value with @JSON.stringify@ so all reference to JSVal is lost and may be expensive when the object is large.
decodeJSON :: (FromJSON a) => JSON -> IO (Maybe a)
decodeJSON =
  fmap (either (const Nothing) Just) . eitherDecodeJSON

-- | NOTE: This converts a value with @JSON.stringify@ so all reference to JSVal is lost and may be expensive when the object is large.
eitherDecodeJSON :: (FromJSON a) => JSON -> IO (Either String a)
eitherDecodeJSON =
  runExceptT . (ExceptT . pure . eitherResult . J.fromJSON <=< ExceptT . parseJSONFromJS)

eitherResult :: J.Result a -> Either String a
{-# INLINE eitherResult #-}
eitherResult = \case
  J.Success a -> Right a
  J.Error e -> Left e

type data JSONObjectClass :: Prototype

type JSONObject = JSObject JSONObjectClass

valueToJSON :: J.Value -> IO JSON
valueToJSON (J.String s) = pure $ unsafeCast $ fromText @USVStringClass s
valueToJSON (J.Number n) = case floatingOrInteger n of
  Left d -> pure $ unsafeCast $ toJSPrim @Double d
  Right i -> pure $ unsafeCast $ toJSPrim @Int i
valueToJSON (J.Bool b) = js_encode_boole b
valueToJSON J.Null = pure $ unsafeCast $ none @JSONClass
valueToJSON (J.Array arr) = unsafeCast . Seq.toSequence <$> mapM valueToJSON arr
valueToJSON (J.Object obj) = do
  dic <- js_new_obj
  void $
    AKM.traverseWithKey
      ( \k v -> do
          val <- evaluate =<< valueToJSON v
          js_set_prop dic (fromText $ AK.toText k) val
      )
      obj
  pure dic

parseJSONFromJS :: JSON -> IO (Either String J.Value)
parseJSONFromJS =
  fmap (Bi.first (\(stk, msg) -> "Error during parsing JS JSON (" <> foldl1 (\l r -> l <> "." <> r) stk <> "): " <> msg))
    . runExceptT
    . go ["$"]
  where
    go stack json
      | js_is_null json = pure J.Null
      | js_is_number json = do
          case fromNullable $ js_decode_int json of
            Just int' -> pure $ J.Number $ fromIntegral $ fromJSPrim int'
            Nothing -> do
              nullable
                (throwE (stack, "JSON: number is neither integral nor double: " <> fromJSString (js_typeof json)))
                (pure . J.Number . realToFrac . fromJSPrim)
                $ js_decode_double json
      | otherwise =
          asum
            [ nullable (throwE (stack, "Not a string")) (pure . J.String . toText)
                =<< liftIO (js_decode_string json)
            , nullable (throwE (stack, "Not a bool")) (pure . J.Bool . fromJSPrim)
                =<< liftIO (js_decode_bool json)
            , nullable
                (throwE (stack, "Not an array"))
                (fmap J.Array . V.imapM (go . (: stack) . show) <=< liftIO . Seq.toVector)
                =<< liftIO (js_decode_array json)
            , do
                obj <-
                  nullable (throwE (stack, "Invalid JSON value: the value of type " <> fromJSString (js_typeof json) <> " given.")) pure
                    =<< liftIO (js_decode_object json)
                props <- liftIO $ Seq.toVector =<< js_props obj
                J.Object . AKM.fromList . F.toList
                  <$> mapM
                    ( \prop -> do
                        let !txt = toText prop
                        val <- liftIO $ js_get_prop obj prop
                        (AK.fromText txt,) <$> go (T.unpack txt : stack) val
                    )
                    props
            ]

foreign import javascript unsafe "typeof $1"
  js_typeof :: JSON -> JSString

foreign import javascript unsafe "if (typeof $1 === 'object') { return $1 } else { return null}"
  js_decode_object :: JSON -> IO (Nullable JSONObjectClass)

foreign import javascript unsafe "Object.getOwnPropertyNames($1)"
  js_props :: JSONObject -> IO (Sequence USVStringClass)

foreign import javascript unsafe "$1[$2]"
  js_get_prop :: JSONObject -> USVString -> IO JSON

foreign import javascript unsafe "$1 === null || $1 === undefined"
  js_is_null :: JSON -> Bool

foreign import javascript unsafe "typeof $1 === 'number'"
  js_is_number :: JSON -> Bool

foreign import javascript unsafe "if (typeof $1 === 'boolean') { return $1 } else { return null }"
  js_decode_bool :: JSON -> IO (Nullable (JSPrimClass Bool))

foreign import javascript unsafe "if (typeof $1 === 'number' && Number.isInteger($1)) { return $1; } else { return null; }"
  js_decode_int :: JSON -> Nullable (JSPrimClass Int)

foreign import javascript unsafe "if (typeof $1 === 'number') { if (Number.isInteger($1)) { return null } else { return $1; } } else { return null; }"
  js_decode_double :: JSON -> Nullable (JSPrimClass Double)

foreign import javascript unsafe "if (typeof $1 === 'string') { return $1; } else { return null; }"
  js_decode_string :: JSON -> IO (Nullable USVStringClass)

foreign import javascript unsafe "if (Array.isArray($1)) { return $1; } else { return null; }"
  js_decode_array :: JSON -> IO (Nullable (SequenceClass JSONClass))

foreign import javascript unsafe "if ($1) { return true } else { return false }"
  js_encode_boole :: Bool -> IO JSON

foreign import javascript unsafe "{}"
  js_new_obj :: IO JSON

foreign import javascript unsafe "$1[$2] = $3"
  js_set_prop :: JSON -> USVString -> JSON -> IO ()
