{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.WASM.JSVal.Convert (
  FromJSVal (..),
  ToJSVal (..),

  -- * Deriving Modifiers
  ViaJSON (..),
  GenericToJSVal,
) where

import Data.Aeson.Micro (ToJSON, Value, toJSON)
import Data.Void (Void)
import GHC.Generics
import GHC.Wasm.Prim
import Language.WASM.JSVal.JSON qualified as JSON
import Wasm.Control.Functor.Linear qualified as Control
import Wasm.Data.Array.Destination.JSVal (DJSArray)
import Wasm.Data.Array.Destination.JSVal qualified as DJSArray
import Wasm.Prelude.Linear qualified as PL
import Wasm.System.IO.Linear qualified as LIO

-- | Convert a value to/from JSVal via JSON de/serialisation with side-effect.
newtype ViaJSON a = ViaJSON {runViaJSON :: a}

instance ToJSVal Value where
  toJSVal x = LIO.fromSystemIO (JSON.toJSVal x)

instance FromJSVal Value where
  fromJSVal = JSON.fromJSVal

instance (ToJSON a) => ToJSVal (ViaJSON a) where
  toJSVal = toJSVal . toJSON . runViaJSON

class FromJSVal a where
  fromJSVal :: JSVal -> IO (Maybe a)

class ToJSVal a where
  toJSVal :: a -> LIO.IO JSVal

instance FromJSVal JSVal where
  fromJSVal = pure . Just
  {-# INLINE fromJSVal #-}

{-
instance ToJSVal JSVal where
  toJSVal = Control.pure
  {-# INLINE toJSVal #-} -}

type GenericToJSVal a = (Generic a, GWriteToObj (Rep a))

instance (GenericToJSVal a) => ToJSVal (Generically a) where
  toJSVal (Generically x) = genericToJSVal x
  {-# INLINE toJSVal #-}

genericToJSVal :: forall a. (GenericToJSVal a) => a -> LIO.IO JSVal
genericToJSVal x = case gwriteToObj @(Rep a) of
  RecEncoder f -> withPartialJSVal \pobj -> f pobj (from x)
  ProdEncoder n f -> Control.do
    DJSArray.JSArray val <- DJSArray.allocIO n (f (from x))
    Control.pure val

newtype PartialJSVal = PartialJSVal JSVal
  deriving newtype (PL.ConsumableM LIO.IO)

foreign import javascript unsafe "Object()"
  js_new_obj :: IO PartialJSVal

withPartialJSVal :: (PartialJSVal %1 -> LIO.IO PartialJSVal) %1 -> LIO.IO JSVal
withPartialJSVal f = Control.do
  pobj <- LIO.fromSystemIO js_new_obj
  PartialJSVal obj <- f pobj
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
    (PartialJSVal %1 -> f Void -> LIO.IO PartialJSVal) %1 ->
    ProductEncoder 'Rec f

class GWriteToObj f where
  gwriteToObj :: ProductEncoder (ProductEncodingOf f) f

foreign import javascript unsafe "$1[$2] = $3; return $1"
  js_set_prop :: PartialJSVal %1 -> JSString -> JSVal %1 -> PartialJSVal

instance
  {-# OVERLAPPING #-}
  (GToJSVal f, Selector ('MetaSel ('Just _m) _x _y _z)) =>
  GWriteToObj (M1 S ('MetaSel ('Just _m) _x _y _z) f)
  where
  gwriteToObj = RecEncoder \pobj (M1 x) -> Control.do
    x' <- gtoJSVal x
    Control.pure
      PL.$ js_set_prop
        pobj
        (toJSString $ selName (undefined :: M1 S ('MetaSel ('Just _m) _x _y _z) f p))
        x'

instance
  {-# OVERLAPPING #-}
  (GToJSVal f) =>
  GWriteToObj (M1 S ('MetaSel 'Nothing _x _y _z) f)
  where
  gwriteToObj = ProdEncoder 1 \fx da -> Control.do
    x' <- gtoJSVal (unM1 fx)
    () <- Control.pure (DJSArray.fill x' da)
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

class GToJSVal f where
  gtoJSVal :: f a -> LIO.IO JSVal

instance (ToJSVal c) => GToJSVal (K1 i c) where
  gtoJSVal = toJSVal . unK1

instance (GToJSVal f) => GToJSVal (M1 i c f) where
  gtoJSVal = gtoJSVal . unM1

class GFromJSVal f where
  gfromJSVal :: JSVal -> IO (Maybe (f a))

instance (FromJSVal c) => GFromJSVal (K1 i c) where
  gfromJSVal = fmap (fmap K1) . fromJSVal

instance
  (GFromJSVal f, GFromJSVal g) =>
  GFromJSVal (f :*: g)
  where
  gfromJSVal = liftA2 (liftA2 (:*:)) <$> gfromJSVal <*> gfromJSVal

foreign import javascript unsafe "$1[$2]"
  js_get_prop :: JSVal -> JSString -> IO JSVal

instance
  {-# OVERLAPPING #-}
  (Selector sel, GFromJSVal f) =>
  GFromJSVal (M1 S sel f)
  where
  gfromJSVal val = do
    let name = selName (undefined :: M1 S sel f p)
    jsVal <- js_get_prop val $ toJSString name
    fmap M1 <$> gfromJSVal jsVal

instance
  {-# OVERLAPPABLE #-}
  ( GFromJSVal f
  ) =>
  GFromJSVal (M1 i c f)
  where
  gfromJSVal = fmap (fmap M1) . gfromJSVal
