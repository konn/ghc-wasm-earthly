{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.WASM.JSVal.Convert (
  FromJSVal (..),
  ToJSVal (..),

  -- * Deriving Modifiers
  ViaJSON (..),
) where

import Control.Monad.IO.Class
import Data.Aeson.Micro (ToJSON, Value, toJSON)
import Data.Array.Destination.Micro (DArray)
import Data.Array.Destination.Micro qualified as DArray
import Data.Void (Void)
import GHC.Generics
import GHC.Wasm.Prim
import Language.WASM.JSVal.JSON qualified as JSON
import System.IO.Unsafe (unsafePerformIO)

-- | Convert a value to/from JSVal via JSON de/serialisation with side-effect.
newtype ViaJSON a = ViaJSON {runViaJSON :: a}

instance ToJSVal Value where
  toJSVal = liftIO . JSON.toJSVal

instance FromJSVal Value where
  fromJSVal = JSON.fromJSVal

instance (ToJSON a) => ToJSVal (ViaJSON a) where
  toJSVal = toJSVal . toJSON . runViaJSON

class FromJSVal a where
  fromJSVal :: JSVal -> IO (Maybe a)

class ToJSVal a where
  toJSVal :: a -> IO JSVal

instance FromJSVal JSVal where
  fromJSVal = pure . Just
  {-# INLINE fromJSVal #-}

instance ToJSVal JSVal where
  toJSVal = pure
  {-# INLINE toJSVal #-}

newtype PartialJSVal = PartialJSVal JSVal

data ProdEncode = Prod | Rec

type family ProductEncodingOf f where
  ProductEncodingOf (l :*: _) = ProductEncodingOf l
  ProductEncodingOf (M1 S ('MetaSel ('Just _) _ _ _) _) = 'Rec
  ProductEncodingOf (M1 S ('MetaSel 'Nothing _ _ _) _) = 'Prod
  ProductEncodingOf (M1 _ _ f) = ProductEncodingOf f

data ProductEncoder pe f where
  ProdEncoder :: !Int -> (f Void -> DArray JSVal %1 -> ()) %1 -> ProductEncoder 'Prod f
  RecEncoder :: (PartialJSVal %1 -> f Void -> PartialJSVal) %1 -> ProductEncoder 'Rec f

class GWriteToObj f where
  gwriteToObj :: ProductEncoder (ProductEncodingOf f) f

foreign import javascript unsafe "$1[$2] = $3; $1"
  js_set_prop :: PartialJSVal %1 -> JSString -> JSVal -> PartialJSVal

instance
  {-# OVERLAPPING #-}
  (GToJSVal f, Selector ('MetaSel ('Just _m) _x _y _z)) =>
  GWriteToObj (M1 S ('MetaSel ('Just _m) _x _y _z) f)
  where
  gwriteToObj = RecEncoder \pobj (M1 x) ->
    js_set_prop
      pobj
      (toJSString $ selName (undefined :: M1 S ('MetaSel ('Just _m) _x _y _z) f p))
      -- FIXME: wrap all IO
      (unsafePerformIO $ gtoJSVal x)

instance
  {-# OVERLAPPING #-}
  (GToJSVal f) =>
  GWriteToObj (M1 S ('MetaSel 'Nothing _x _y _z) f)
  where
  gwriteToObj = ProdEncoder 1 \fx da ->
    DArray.fill
      -- FIXME: wrap all IO
      (unsafePerformIO $ gtoJSVal $ unM1 fx)
      da

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
          case DArray.split n dest of
            (lh, rh) -> case f l lh of
              () -> g r rh
      (RecEncoder f, RecEncoder g) ->
        RecEncoder \pobj (fx :*: gx) -> g (f pobj fx) gx

class GToJSVal f where
  gtoJSVal :: f a -> IO JSVal

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
    jsVal <- liftIO $ js_get_prop val $ toJSString name
    fmap M1 <$> gfromJSVal jsVal

instance
  {-# OVERLAPPABLE #-}
  ( GFromJSVal f
  ) =>
  GFromJSVal (M1 i c f)
  where
  gfromJSVal = fmap (fmap M1) . gfromJSVal
