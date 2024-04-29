{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Aeson.Micro.Generics (
  GenericFromJSON,
  genericParseJSON,
  GenericToJSON,
  genericToJSON,
) where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Data.Aeson.Micro
import Data.Aeson.Micro qualified as J
import Data.Coerce (coerce)
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Generics

data Alt = Affine | Multi

type GFromJSON = GFromJSON' Affine

type GFromJSON' :: Alt -> (Type -> Type) -> Constraint
class GFromJSON' n f where
  gparseJSON :: Value -> Parser (f a)

class GFromJSONField f where
  gparseJSONField :: Maybe Value -> Parser (f a)

type GToJSON' :: Alt -> (Type -> Type) -> Constraint
class GToJSON' m f where
  gtoJSON :: f a -> Value

type GWithObject' :: Alt -> (Type -> Type) -> Constraint
class GWithObject' m f where
  gwithObject :: f a -> Object -> Object

instance
  ( GWithObject' 'Multi f
  , GWithObject' 'Multi g
  ) =>
  GWithObject' m (f :+: g)
  where
  gwithObject (L1 f) = gwithObject @'Multi f
  gwithObject (R1 g) = gwithObject @'Multi g
  {-# INLINE gwithObject #-}

instance (GWithObject' 'Affine f, GWithObject' 'Affine g) => GWithObject' Affine (f :*: g) where
  gwithObject (f :*: g) = gwithObject @Affine g . gwithObject @Affine f
  {-# INLINE gwithObject #-}

instance {-# OVERLAPPABLE #-} (GToJSON' m f) => GToJSON' m (M1 i c f) where
  gtoJSON (M1 f) = gtoJSON @m f
  {-# INLINE gtoJSON #-}

instance
  {-# OVERLAPPING #-}
  (GToJSON' Affine f, Constructor c) =>
  GWithObject' Multi (M1 C c f)
  where
  gwithObject (M1 f) =
    let con = conName (undefined :: M1 C c f a)
     in Map.insert (T.pack con) $ gtoJSON @Affine f
  {-# INLINE gwithObject #-}

instance {-# OVERLAPPABLE #-} (GWithObject' p f) => GWithObject' p (M1 i c f) where
  gwithObject (M1 f) = gwithObject @p f
  {-# INLINE gwithObject #-}

instance
  {-# OVERLAPPING #-}
  (Selector ('MetaSel ('Just n) x b c), GToJSON' Affine f) =>
  GWithObject' 'Affine (M1 S ('MetaSel ('Just n) x b c) f)
  where
  gwithObject (M1 f) = Map.insert (T.pack $ selName (undefined :: M1 S ('MetaSel ('Just n) x b c) f a)) (gtoJSON @Affine f)
  {-# INLINE gwithObject #-}

instance
  {-# OVERLAPPING #-}
  (FromJSON a) =>
  GFromJSONField (K1 i (Maybe a))
  where
  gparseJSONField Nothing = pure $ K1 Nothing
  gparseJSONField (Just v) = K1 . Just <$> parseJSON v
  {-# INLINE gparseJSONField #-}

instance (FromJSON a) => GFromJSONField (K1 i a) where
  gparseJSONField (Just v) = K1 <$> parseJSON v
  gparseJSONField Nothing = fail "missing field"
  {-# INLINE gparseJSONField #-}

instance (ToJSON a) => GToJSON' Affine (K1 i a) where
  gtoJSON = toJSON . unK1
  {-# INLINE gtoJSON #-}

instance (GFromJSON' Affine f, Constructor c) => GFromJSON' 'Multi (M1 C c f) where
  gparseJSON (Object dic) =
    let con = conName (undefined :: M1 C c f a)
     in case Map.lookup (T.pack con) dic of
          Just v -> M1 <$> gparseJSON @Affine v
          Nothing -> fail "No"
  gparseJSON _ = fail "expected object"
  {-# INLINE gparseJSON #-}

instance (FromJSON c) => GFromJSON' m (K1 i c) where
  gparseJSON = fmap K1 . parseJSON

instance
  {-# OVERLAPPING #-}
  (GFromJSONField f, GFromJSON' Affine f, Selector sel) =>
  GFromJSON' 'Affine (M1 S sel f)
  where
  gparseJSON (Object dic)
    | let sel = T.pack $ selName (undefined :: M1 S sel f a)
    , not $ T.null sel =
        M1 <$> gparseJSONField (Map.lookup sel dic)
  gparseJSON v = M1 <$> gparseJSON @Affine v
  {-# INLINE gparseJSON #-}

instance
  ( GFromJSON' 'Multi f
  , GFromJSON' 'Multi g
  ) =>
  GFromJSON' m (f :+: g)
  where
  gparseJSON v =
    maybe (fail "No") pure $
      (L1 <$> parseMaybe (gparseJSON @Multi) v)
        <|> (R1 <$> parseMaybe (gparseJSON @Multi) v)
  {-# INLINE gparseJSON #-}

instance (GFromJSON' 'Affine f, GFromJSON' 'Affine g) => GFromJSON' Affine (f :*: g) where
  gparseJSON v = (:*:) <$> gparseJSON @Affine v <*> gparseJSON @Affine v
  {-# INLINE gparseJSON #-}

type GenericFromJSON a = (Generic a, GFromJSON (Rep a))

genericParseJSON :: (GenericFromJSON a) => Value -> Parser a
genericParseJSON = fmap to . gparseJSON @Affine

type GWithObject = GWithObject' 'Affine

type GenericToJSON a = (Generic a, GWithObject (Rep a))

genericToJSON :: (GenericToJSON a) => a -> Value
genericToJSON a = Object $ gwithObject @Affine (from a) Map.empty

instance (GenericFromJSON a) => FromJSON (Generically a) where
  parseJSON = fmap Generically . genericParseJSON
  {-# INLINE parseJSON #-}

instance (GenericToJSON a) => ToJSON (Generically a) where
  toJSON = genericToJSON . coerce @_ @a
  {-# INLINE toJSON #-}

instance (FromJSON a) => FromJSON (V.Vector a) where
  parseJSON = fmap V.fromList <$> parseJSON
  {-# INLINE parseJSON #-}

instance (ToJSON a) => ToJSON (V.Vector a) where
  toJSON = toJSON . V.toList
  {-# INLINE toJSON #-}

instance (FromJSON a) => FromJSON (NonEmpty a) where
  parseJSON = maybe (fail "no") pure . NE.nonEmpty <=< parseJSON
  {-# INLINE parseJSON #-}

instance (GFromJSON' m f) => GFromJSON' m (M1 D c f) where
  gparseJSON = fmap M1 . gparseJSON @m
  {-# INLINE gparseJSON #-}

instance GFromJSON' 'Affine U1 where
  gparseJSON v = do
    [] :: [Value] <- parseJSON v
    pure U1
  {-# INLINE gparseJSON #-}

instance GToJSON' 'Affine U1 where
  gtoJSON U1 = J.Array []
  {-# INLINE gtoJSON #-}

instance (ToJSON a) => ToJSON (NonEmpty a) where
  toJSON = toJSON . NE.toList
  {-# INLINE toJSON #-}
