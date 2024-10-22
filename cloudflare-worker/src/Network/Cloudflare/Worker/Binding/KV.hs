{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Network.Cloudflare.Worker.Binding.KV (
  KV,
  KVClass,
  delete,
  listKeys,
  ListKeys (..),
  ListKeyResult (..),
  Key (..),
  get,
  getWithMetadata,
  ValueWithMetadata (..),
  put,
  PutOptions (..),
  Cursor (..),
) where

import Control.Monad (forM, void, (<=<))
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Aeson qualified as J
import Data.Bifunctor qualified as Bi
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Prim
import GHC.Wasm.Web.JSON
import Wasm.Prelude.Linear qualified as PL

----------------
-- Core types
----------------

type data KVClass :: Prototype

type instance SuperclassOf KVClass = 'Nothing

type KV = JSObject KVClass

----------------
-- Delete
----------------

delete :: KV -> String -> IO ()
delete kv = void . await <=< js_kv_delete kv . toJSString

foreign import javascript safe "$1.delete($2)"
  js_kv_delete :: KV -> JSString -> IO (Promise UndefinedClass)

----------------
-- List keys
----------------

data ListKeys = ListKeys
  { prefix :: !(Maybe String)
  , limit :: !(Maybe Word32)
  , cursor :: !(Maybe Cursor)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype Cursor = Cursor {cursor :: String}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

type JSListKeyInit =
  JSDictionary
    '[ '("prefix", NullableClass USVStringClass)
     , '("limit", NullableClass (JSPrimClass Word32))
     , '("cursor", NullableClass USVStringClass)
     ]

data ListKeyResult = ListKeyResult
  { keys :: ![Key]
  , cursor :: !(Maybe Cursor)
  , list_complete :: !Bool
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Key = Key
  { name :: String
  , expiration :: Maybe Word32
  , metadata :: Maybe Value
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

listKeys :: KV -> ListKeys -> IO (Either String ListKeyResult)
listKeys kv key = do
  let jsKey = fromListKey key
  json <- await =<< js_kv_list kv jsKey
  mval <- decodeJSON json
  case mval of
    Just val -> pure $ Bi.first ("Invalid JSON: " <>) $ eitherResult $ J.fromJSON val
    Nothing -> pure $ Left "Failed to decode as a JSON"

eitherResult :: J.Result a -> Either String a
eitherResult (J.Success a) = Right a
eitherResult (J.Error e) = Left e

fromListKey :: ListKeys -> JSListKeyInit
{-# NOINLINE fromListKey #-}
fromListKey ListKeys {..} =
  let prefix' = toUSVString . toJSString <$> prefix
      limit' = toJSPrim <$> limit
      cursor' = toUSVString . toJSString . (.cursor) <$> cursor
   in newDictionary
        ( setPartialField "cursor" (toNullable cursor')
            PL.. setPartialField "prefix" (toNullable prefix')
            PL.. setPartialField "limit" (toNullable limit')
        )

foreign import javascript safe "$1.list($2)"
  js_kv_list :: KV -> JSListKeyInit -> IO (Promise JSONClass)

----------------
-- Get
----------------

get :: KV -> String -> IO (Maybe String)
get kv key = do
  fmap
    (fmap (fromJSString . fromUSVString) . fromNullable)
    . await
    =<< js_kv_get kv (toJSString key)

foreign import javascript safe "$1.get($2)"
  js_kv_get :: KV -> JSString -> IO (Promise (NullableClass USVStringClass))

data ValueWithMetadata = ValueWithMetadata
  { value :: String
  , metadata :: Maybe Value
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

getWithMetadata :: KV -> String -> IO (Maybe ValueWithMetadata)
getWithMetadata kv key = do
  mjson <- await =<< js_kv_get_with_metadata kv (toJSString key)
  forM (fromNullable mjson) \json -> do
    mval <- decodeJSON json
    case mval of
      Nothing -> error "Failed to decode as a JSON"
      Just val ->
        either (error . ("Invalid JSON: " <>)) pure $
          eitherResult $
            J.fromJSON val

foreign import javascript safe "$1.getWithMetadata($2)"
  js_kv_get_with_metadata :: KV -> JSString -> IO (Promise (NullableClass JSONClass))

----------------
-- Put
----------------

data PutOptions = PutOptions
  { expiration :: Maybe Word32
  , expirationTtl :: Maybe Word32
  , metadata :: Maybe Value
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON)

instance J.ToJSON PutOptions where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}

put :: KV -> PutOptions -> String -> String -> IO ()
put kv opts key value = do
  jsOpts <- encodeJSON $ J.toJSON opts
  void $ await =<< js_kv_put kv jsOpts (toJSString key) (toJSString value)

foreign import javascript safe "$1.put($3, $4, $2)"
  js_kv_put :: KV -> JSON -> JSString -> JSString -> IO (Promise UndefinedClass)
