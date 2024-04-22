{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Aeson.Micro.Generics (
  GenericFromJSON,
  genericParseJSON,
  GenericToJSON,
  genericToJSON,
) where

import Data.Aeson.Micro
import Data.Coerce (coerce)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Generics

class GFromJSON f where
  gparseJSON :: Value -> Parser (f a)

class GFromJSONField f where
  gparseJSONField :: Maybe Value -> Parser (f a)

class GToJSON f where
  gtoJSON :: f a -> Value

class GWithObject f where
  gwithObject :: f a -> Object -> Object

instance (GWithObject f, GWithObject g) => GWithObject (f :*: g) where
  gwithObject (f :*: g) = gwithObject g . gwithObject f
  {-# INLINE gwithObject #-}

instance {-# OVERLAPPABLE #-} (GToJSON f) => GToJSON (M1 i c f) where
  gtoJSON (M1 f) = gtoJSON f
  {-# INLINE gtoJSON #-}

instance {-# OVERLAPPABLE #-} (GWithObject f) => GWithObject (M1 i c f) where
  gwithObject (M1 f) = gwithObject f
  {-# INLINE gwithObject #-}

instance
  {-# OVERLAPPING #-}
  (Selector sel, GToJSON f) =>
  GWithObject (M1 S sel f)
  where
  gwithObject (M1 f) = Map.insert (T.pack $ selName (undefined :: M1 S sel f a)) (gtoJSON f)
  {-# INLINE gwithObject #-}

instance
  {-# OVERLAPPING #-}
  (FromJSON a) =>
  GFromJSONField (K1 i (Maybe a))
  where
  gparseJSONField Nothing = pure $ K1 Nothing
  gparseJSONField (Just v) = K1 . Just <$> parseJSON v
  {-# INLINE gparseJSONField #-}

instance
  {-# OVERLAPPABLE #-}
  (FromJSON a) =>
  GFromJSONField (K1 i a)
  where
  gparseJSONField (Just v) = K1 <$> parseJSON v
  gparseJSONField Nothing = fail "missing field"
  {-# INLINE gparseJSONField #-}

instance (ToJSON a) => GToJSON (K1 i a) where
  gtoJSON = toJSON . unK1
  {-# INLINE gtoJSON #-}

instance {-# OVERLAPPABLE #-} (GFromJSON f) => GFromJSON (M1 i c f) where
  gparseJSON = fmap M1 . gparseJSON
  {-# INLINE gparseJSON #-}

instance
  {-# OVERLAPPING #-}
  (GFromJSONField f, Selector sel) =>
  GFromJSON (M1 S sel f)
  where
  gparseJSON (Object dic) =
    M1 <$> gparseJSONField (Map.lookup (T.pack $ selName (undefined :: M1 S sel f a)) dic)
  gparseJSON _ = fail "expected object"
  {-# INLINE gparseJSON #-}

instance (GFromJSON f, GFromJSON g) => GFromJSON (f :*: g) where
  gparseJSON v = (:*:) <$> gparseJSON v <*> gparseJSON v
  {-# INLINE gparseJSON #-}

type GenericFromJSON a = (Generic a, GFromJSON (Rep a))

genericParseJSON :: (GenericFromJSON a) => Value -> Parser a
genericParseJSON = fmap to . gparseJSON

type GenericToJSON a = (Generic a, GWithObject (Rep a))

genericToJSON :: (GenericToJSON a) => a -> Value
genericToJSON a = Object $ gwithObject (from a) Map.empty

instance (GenericFromJSON a) => FromJSON (Generically a) where
  parseJSON = fmap Generically . genericParseJSON
  {-# INLINE parseJSON #-}

instance (GenericToJSON a) => ToJSON (Generically a) where
  toJSON = genericToJSON . coerce @_ @a
  {-# INLINE toJSON #-}
