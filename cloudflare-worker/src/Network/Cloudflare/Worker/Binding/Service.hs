{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Service Binding for custom RPC between Cloudflare Workers.
module Network.Cloudflare.Worker.Binding.Service (
  Fun (..),
  ServiceClass,
  Service,
  call,
  ToServiceBinding (..),
  genericToServiceBinding,
  GenericToServiceBinding,
  ToJSFun (..),
  KnownJSFun (..),
  FromFun,
  IsServiceArg (..),
  ViaJSPrim (..),
  ViaJSON (..),
) where

import Control.Monad (join, (<=<))
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Int
import Data.Kind (Constraint, Type)
import Data.Vector qualified as V
import Data.Word
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics
import GHC.Records
import GHC.TypeError
import GHC.TypeLits
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Prim
import GHC.Wasm.Web.JSON
import Language.WASM.JSVal.Convert

type data Fun = Return Prototype | Prototype :~> Fun

infixr 5 :~>

type family FromFun fun where
  FromFun (Return f) = IO (Promise f)
  FromFun (f :~> fun) = JSObject f -> FromFun fun

type data ServiceClass :: [(Symbol, Fun)] -> Prototype

type Service fs = JSObject (ServiceClass fs)

type data JSFunClass :: Fun -> Prototype

type JSFun fn = JSObject (JSFunClass fn)

instance
  ( KnownSymbol l
  , Member l fs
  , x ~ FromFun (Lookup' l fs)
  , KnownJSFun (Lookup' l fs)
  ) =>
  HasField l (Service fs) x
  where
  getField = call l

call ::
  forall fs x.
  forall l ->
  ( KnownSymbol l
  , Member l fs
  , x ~ FromFun (Lookup' l fs)
  , KnownJSFun (Lookup' l fs)
  ) =>
  Service fs ->
  x
call l svs =
  joinFun @(Lookup' l fs) proxy# $ do
    unmarshalFun <$> js_cal_raw @fs @(Lookup' l fs) svs (toJSString $ symbolVal' @l proxy#)

{- | N.B. Generic class involves curry/uncurrying on each function argument.
If you have a function with many arguments, this can lead to performance degression.
In such cases, we recommend to write the instance manually.
-}
class ToServiceBinding a where
  type Signature a :: [(Symbol, Fun)]
  type Signature a = GSignature (Rep a)
  toServiceBinding :: a -> IO (JSObject (ServiceClass (Signature a)))
  default toServiceBinding ::
    (GenericToServiceBinding a) =>
    a ->
    IO (JSObject (ServiceClass (Signature a)))
  toServiceBinding = genericToServiceBinding

type GenericToServiceBinding a = (Generic a, GToServiceBinding (Rep a), Signature a ~ GSignature (Rep a))

genericToServiceBinding ::
  (GenericToServiceBinding a) =>
  a ->
  IO (JSObject (ServiceClass (GSignature (Rep a))))
genericToServiceBinding a = do
  sink <- js_new_service_sink
  gwriteField sink $ from a
  pure sink.runServiceSink

class GToServiceBinding f where
  type GSignature f :: [(Symbol, Fun)]
  gwriteField :: ServiceSink (GSignature f) -> f a -> IO ()

castSink :: forall fs fs0. ServiceSink fs0 -> ServiceSink fs
castSink = ServiceSink . unsafeCast . (.runServiceSink)

instance (KnownSymbol l, ToJSFun c) => GToServiceBinding (S1 ('MetaSel ('Just l) _pk _st _d) (K1 i c)) where
  type GSignature (S1 ('MetaSel ('Just l) _pk _st _d) (K1 i c)) = '[ '(l, JSFunOf c)]
  gwriteField sink (M1 (K1 x)) = toJSFun x >>= js_set_handler sink (toJSString $ symbolVal' @l proxy#)

instance (GToServiceBinding f) => GToServiceBinding (D1 i f) where
  type GSignature (D1 i f) = GSignature f
  gwriteField sink (M1 x) = gwriteField sink x

instance (GToServiceBinding f) => GToServiceBinding (C1 i f) where
  type GSignature (C1 i f) = GSignature f
  gwriteField sink (M1 x) = gwriteField sink x

instance (GToServiceBinding f, GToServiceBinding g) => GToServiceBinding (f :*: g) where
  type GSignature (f :*: g) = GSignature f ++ GSignature g
  gwriteField sink (f :*: g) = gwriteField (castSink @(GSignature f) sink) f >> gwriteField (castSink @(GSignature g) sink) g

instance (Unsatisfiable ('Text "Sum type cannot be marshaled into a service binding")) => GToServiceBinding (f :+: g) where
  type GSignature (f :+: g) = TypeError ('Text "Sum type cannot be marshaled into a service binding")

type family ls ++ rs where
  '[] ++ rs = rs
  (l ': ls) ++ rs = l ': (ls ++ rs)

toJSFun :: (ToJSFun a) => a -> IO (JSFun (JSFunOf a))
toJSFun = marshalJSFun . toFromFun

type ToJSFun :: Type -> Constraint
class (KnownJSFun (JSFunOf a)) => ToJSFun a where
  toFromFun :: a -> FromFun (JSFunOf a)

type family AsJSRet a where
  AsJSRet (Promise a) = a
  AsJSRet (JSObject a) = a
  AsJSRet a = JSPrimClass a

type family JSFunOf a where
  JSFunOf (IO a) = Return (AsJSRet a)
  JSFunOf (a -> b) = AsJSArg a :~> JSFunOf b

type family AsJSArg a where
  AsJSArg (JSObject a) = a
  AsJSArg a = JSPrimClass a

instance {-# OVERLAPPING #-} ToJSFun (IO (Promise c)) where
  toFromFun = id

instance {-# OVERLAPS #-} (AsJSRet (JSObject c) ~ c) => ToJSFun (IO (JSObject c)) where
  toFromFun = (newPromise =<<)

instance
  {-# OVERLAPPABLE #-}
  ( AsJSRet c ~ JSPrimClass c
  , JSPrimitive c
  ) =>
  ToJSFun (IO c)
  where
  toFromFun = (newPromise . toJSPrim =<<)

instance {-# OVERLAPPING #-} (ToJSFun b) => ToJSFun (JSObject a -> b) where
  toFromFun f = \x -> toFromFun $ f x

instance
  {-# OVERLAPPABLE #-}
  ( ToJSFun b
  , AsJSArg a ~ JSPrimClass a
  , JSPrimitive a
  ) =>
  ToJSFun (a -> b)
  where
  toFromFun f = \x -> toFromFun $ f $ fromJSPrim x

type KnownJSFun :: Fun -> Constraint
class KnownJSFun fn where
  marshalJSFun :: FromFun fn -> IO (JSObject (JSFunClass fn))
  unmarshalFun :: JSFun fn -> FromFun fn
  joinFun :: Proxy# fn -> IO (FromFun fn) -> FromFun fn

instance KnownJSFun (Return f) where
  marshalJSFun = coerce
  unmarshalFun = js_return_fun
  joinFun _ = join

instance (KnownJSFun fs) => KnownJSFun (f :~> fs) where
  marshalJSFun f = js_ffi_fun_arrow @f @fs $ marshalJSFun . f
  unmarshalFun jsf x = unmarshalFun $ js_ffi_app_fun jsf x
  joinFun _ f x = joinFun @fs (proxy#) $ ($ x) <$> f

class IsServiceArg a where
  type AsServiceArg a :: Prototype
  toServiceArg :: a -> IO (JSObject (AsServiceArg a))
  parseServiceArg :: JSObject (AsServiceArg a) -> IO (Either String a)

instance (FromJSON a, ToJSON a) => IsServiceArg (ViaJSON a) where
  type AsServiceArg (ViaJSON a) = JSONClass
  toServiceArg = encodeJSON . runViaJSON
  {-# INLINE toServiceArg #-}
  parseServiceArg = fmap (fmap ViaJSON) . eitherDecodeJSON
  {-# INLINE parseServiceArg #-}

instance IsServiceArg (JSObject a) where
  type AsServiceArg (JSObject a) = a
  toServiceArg = pure
  {-# INLINE toServiceArg #-}
  parseServiceArg = pure . Right
  {-# INLINE parseServiceArg #-}

newtype ViaJSPrim a = ViaJSPrim {runViaJSPrim :: a}

instance (JSPrimitive a) => IsServiceArg (ViaJSPrim a) where
  type AsServiceArg (ViaJSPrim a) = JSPrimClass a
  toServiceArg = pure . toJSPrim . (.runViaJSPrim)
  {-# INLINE toServiceArg #-}
  parseServiceArg = pure . Right . ViaJSPrim . fromJSPrim
  {-# INLINE parseServiceArg #-}

instance (IsServiceArg a) => IsServiceArg (Maybe a) where
  type AsServiceArg (Maybe a) = NullableClass (AsServiceArg a)
  toServiceArg = maybe (pure none) (fmap nonNull . toServiceArg)
  {-# INLINE toServiceArg #-}
  parseServiceArg = nullable (pure $ Right Nothing) (fmap (fmap Just) . parseServiceArg)
  {-# INLINE parseServiceArg #-}

instance (IsServiceArg a) => IsServiceArg (V.Vector a) where
  type AsServiceArg (V.Vector a) = SequenceClass (AsServiceArg a)
  toServiceArg = fmap toSequence . mapM toServiceArg
  {-# INLINE toServiceArg #-}
  parseServiceArg = runExceptT . mapM (ExceptT . parseServiceArg) <=< toVector
  {-# INLINE parseServiceArg #-}

instance (IsServiceArg a) => IsServiceArg [a] where
  type AsServiceArg [a] = SequenceClass (AsServiceArg a)
  toServiceArg = toServiceArg . V.fromList
  {-# INLINE toServiceArg #-}
  parseServiceArg = fmap (fmap V.toList) . parseServiceArg
  {-# INLINE parseServiceArg #-}

deriving via ViaJSPrim Int instance IsServiceArg Int

deriving via ViaJSPrim Int8 instance IsServiceArg Int8

deriving via ViaJSPrim Int16 instance IsServiceArg Int16

deriving via ViaJSPrim Int32 instance IsServiceArg Int32

deriving via ViaJSPrim Int64 instance IsServiceArg Int64

deriving via ViaJSPrim Word instance IsServiceArg Word

deriving via ViaJSPrim Word8 instance IsServiceArg Word8

deriving via ViaJSPrim Word16 instance IsServiceArg Word16

deriving via ViaJSPrim Word32 instance IsServiceArg Word32

deriving via ViaJSPrim Word64 instance IsServiceArg Word64

deriving via ViaJSPrim Bool instance IsServiceArg Bool

deriving via ViaJSPrim Double instance IsServiceArg Double

deriving via ViaJSPrim Float instance IsServiceArg Float

instance GToServiceBinding U1 where
  type GSignature U1 = '[]
  gwriteField _ _ = pure ()

newtype ServiceSink fs = ServiceSink {runServiceSink :: Service fs}

foreign import javascript unsafe "$1.$2 = $3"
  js_set_handler :: ServiceSink fs -> JSString -> JSFun e -> IO ()

foreign import javascript unsafe "{}"
  js_new_service_sink :: IO (ServiceSink fs)

foreign import javascript "wrapper"
  js_ffi_fun_arrow :: (JSObject f -> IO (JSFun fs)) -> IO (JSFun (f :~> fs))

foreign import javascript unsafe "$1($2)"
  js_ffi_app_fun :: JSFun (f :~> fs) -> JSObject f -> JSFun fs

foreign import javascript unsafe "$1"
  js_return_fun :: JSFun (Return f) -> IO (Promise f)

foreign import javascript unsafe "$1[$2]"
  js_cal_raw :: Service fs -> JSString -> IO (JSFun f)
