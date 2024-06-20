{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.WASM.JSVal.Convert (
  FromWasmJSON (..),
  ToWasmJSON (..),

  -- * Deriving Modifiers
  ViaJSON (..),
  GenericToWasmJSON,
) where

import Data.Aeson (ToJSON, Value, toJSON)
import Data.Coerce (coerce)
import Data.Void (Void)
import GHC.Generics
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Prim
import GHC.Wasm.Web.JSON (JSON)
import GHC.Wasm.Web.JSON qualified as JSON
import Wasm.Control.Functor.Linear qualified as Control
import Wasm.Data.Array.Destination.JSVal (DJSArray)
import Wasm.Data.Array.Destination.JSVal qualified as DJSArray
import Wasm.Prelude.Linear qualified as PL
import Wasm.System.IO.Linear qualified as LIO
import Wasm.Unsafe.Linear qualified as Unsafe

-- | Convert a value to/from JSON via JSON de/serialisation with side-effect.
newtype ViaJSON a = ViaJSON {runViaJSON :: a}

instance ToWasmJSON Value where
  toWasmJSON x = LIO.fromSystemIO (JSON.encodeJSON x)

instance FromWasmJSON Value where
  fromWasmJSON = JSON.decodeJSON

instance (ToJSON a) => ToWasmJSON (ViaJSON a) where
  toWasmJSON = toWasmJSON . toJSON . runViaJSON

class FromWasmJSON a where
  fromWasmJSON :: JSON -> IO (Maybe a)

class ToWasmJSON a where
  toWasmJSON :: a -> LIO.IO JSON

instance FromWasmJSON JSON where
  fromWasmJSON = pure . Just
  {-# INLINE fromWasmJSON #-}

{-
instance ToWasmJSON JSON where
  toWasmJSON = Control.pure
  {-# INLINE toWasmJSON #-} -}

type GenericToWasmJSON a = (Generic a, GWriteToObj (Rep a))

instance (GenericToWasmJSON a) => ToWasmJSON (Generically a) where
  toWasmJSON (Generically x) = genericToWasmJSON x
  {-# INLINE toWasmJSON #-}

genericToWasmJSON :: forall a. (GenericToWasmJSON a) => a -> LIO.IO JSON
genericToWasmJSON x = case gwriteToObj @(Rep a) of
  RecEncoder f -> withPartialJSON \pobj -> f pobj (from x)
  ProdEncoder n f -> Control.do
    DJSArray.JSArray val <- DJSArray.allocIO n (f (from x))
    Control.pure (Unsafe.toLinear unsafeAsObject val)

newtype PartialJSON = PartialJSON JSON
  deriving (PL.ConsumableM LIO.IO) via JSVal

foreign import javascript unsafe "Object()"
  js_new_obj :: IO PartialJSON

withPartialJSON :: (PartialJSON %1 -> LIO.IO PartialJSON) %1 -> LIO.IO JSON
withPartialJSON f = Control.do
  pobj <- LIO.fromSystemIO js_new_obj
  PartialJSON obj <- f pobj
  Control.pure obj

data ProdEncode = Prod | Rec

type family ProductEncodingOf f where
  ProductEncodingOf (l :*: _) = ProductEncodingOf l
  ProductEncodingOf (M1 S ('MetaSel ('Just _) _ _ _) _) = 'Rec
  ProductEncodingOf (M1 S ('MetaSel 'Nothing _ _ _) _) = 'Prod
  ProductEncodingOf (M1 _ _ f) = ProductEncodingOf f

data ProductEncoder pe f where
  ProdEncoder ::
    !Int ->
    (f Void -> DJSArray %1 -> LIO.IO ()) %1 ->
    ProductEncoder 'Prod f
  RecEncoder ::
    (PartialJSON %1 -> f Void -> LIO.IO PartialJSON) %1 ->
    ProductEncoder 'Rec f

class GWriteToObj f where
  gwriteToObj :: ProductEncoder (ProductEncodingOf f) f

foreign import javascript unsafe "$1[$2] = $3; return $1"
  js_set_prop :: PartialJSON %1 -> JSString -> JSON %1 -> PartialJSON

instance
  {-# OVERLAPPING #-}
  (GToWasmJSON f, Selector ('MetaSel ('Just _m) _x _y _z)) =>
  GWriteToObj (M1 S ('MetaSel ('Just _m) _x _y _z) f)
  where
  gwriteToObj = RecEncoder \pobj (M1 x) -> Control.do
    x' <- gtoWasmJSON x
    Control.pure
      PL.$ js_set_prop
        pobj
        (toJSString $ selName (undefined :: M1 S ('MetaSel ('Just _m) _x _y _z) f p))
        x'

instance
  {-# OVERLAPPING #-}
  (GToWasmJSON f) =>
  GWriteToObj (M1 S ('MetaSel 'Nothing _x _y _z) f)
  where
  gwriteToObj = ProdEncoder 1 \fx da -> Control.do
    x' <- gtoWasmJSON (unM1 fx)
    () <- Control.pure (DJSArray.fill (Unsafe.toLinear coerce x') da)
    Control.pure ()

instance {-# OVERLAPPABLE #-} (GWriteToObj f) => GWriteToObj (M1 D c f) where
  gwriteToObj = case gwriteToObj @f of
    ProdEncoder n f -> ProdEncoder n \fx da -> f (unM1 fx) da
    RecEncoder f -> RecEncoder \pobj fx -> f pobj (unM1 fx)

instance {-# OVERLAPPABLE #-} (GWriteToObj f) => GWriteToObj (M1 C c f) where
  gwriteToObj = case gwriteToObj @f of
    ProdEncoder n f -> ProdEncoder n \fx da -> f (unM1 fx) da
    RecEncoder f -> RecEncoder \pobj fx -> f pobj (unM1 fx)

instance
  (GWriteToObj f, GWriteToObj g, ProductEncodingOf f ~ ProductEncodingOf g) =>
  GWriteToObj (f :*: g)
  where
  gwriteToObj =
    case (gwriteToObj @f, gwriteToObj @g) of
      (ProdEncoder n f, ProdEncoder m g) ->
        ProdEncoder (n + m) $ \(l :*: r) dest ->
          case DJSArray.splitAt n dest of
            (lh, rh) -> Control.do
              () <- f l lh
              g r rh
      (RecEncoder f, RecEncoder g) ->
        RecEncoder \pobj (fx :*: gx) -> Control.do
          ls <- f pobj fx
          g ls gx

class GToWasmJSON f where
  gtoWasmJSON :: f a -> LIO.IO JSON

instance (ToWasmJSON c) => GToWasmJSON (K1 i c) where
  gtoWasmJSON = toWasmJSON . unK1

instance (GToWasmJSON f) => GToWasmJSON (M1 i c f) where
  gtoWasmJSON = gtoWasmJSON . unM1

class GFromWasmJSON f where
  gfromWasmJSON :: JSON -> IO (Maybe (f a))

instance (FromWasmJSON c) => GFromWasmJSON (K1 i c) where
  gfromWasmJSON = fmap (fmap K1) . fromWasmJSON

instance
  (GFromWasmJSON f, GFromWasmJSON g) =>
  GFromWasmJSON (f :*: g)
  where
  gfromWasmJSON = liftA2 (liftA2 (:*:)) <$> gfromWasmJSON <*> gfromWasmJSON

foreign import javascript unsafe "$1[$2]"
  js_get_prop :: JSON -> JSString -> IO JSON

instance
  {-# OVERLAPPING #-}
  (Selector sel, GFromWasmJSON f) =>
  GFromWasmJSON (M1 S sel f)
  where
  gfromWasmJSON val = do
    let name = selName (undefined :: M1 S sel f p)
    jsval <- js_get_prop val $ toJSString name
    fmap M1 <$> gfromWasmJSON jsval

instance
  {-# OVERLAPPABLE #-}
  ( GFromWasmJSON f
  ) =>
  GFromWasmJSON (M1 i c f)
  where
  gfromWasmJSON = fmap (fmap M1) . gfromWasmJSON
