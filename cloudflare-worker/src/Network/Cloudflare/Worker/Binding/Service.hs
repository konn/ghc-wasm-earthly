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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Service Binding for custom RPC between Cloudflare Workers.
module Network.Cloudflare.Worker.Binding.Service (
  FunSig (..),
  type (~>),
  ToWorkerFun,
  ToCallerFun,
  IsServiceFunSig (..),
  IsWorkerFun (..),
  ServiceBindingException (..),
  ServiceClass,
  Service,
  call,
  ToService (..),
  genericToService,
  GenericToService,
  IsServiceArg (..),
  ViaJSPrim (..),
  ViaJSON (..),

  -- * Internal
  FromFunSig,
  FunMode (..),
  ToHsRet,
) where

import Control.Exception.Safe (Exception, throwIO)
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

type data FunSig = Return Type | Type :~> FunSig

type data JSFunSig = ReturnJS Prototype | Prototype :~>> JSFunSig

call ::
  forall bs x.
  forall l ->
  (KnownSymbol l, Member l bs, Lookup l bs ~ 'Just x, IsServiceFunSig x) =>
  Service bs ->
  ToCallerFun x
call l srv =
  joinHsFun Caller x $
    decodeFun# (proxy# @x)
      <$> js_get_fun @bs @(ToJSFunSig x) srv (toJSString $ symbolVal' @l proxy#)

instance
  ( KnownSymbol l
  , Member l bs
  , Lookup l bs ~ 'Just x
  , IsServiceFunSig x
  , a ~ ToCallerFun x
  ) =>
  HasField l (Service bs) a
  where
  getField = call l
  {-# INLINE getField #-}

type (~>) = (:~>)

infixr 5 :~>, :~>>, ~>

data FunMode = Caller | Defn

data ServiceBindingException
  = FunResultDecodeFailure !String
  | FunArgDecodeFailure !String
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

type family ToHsRet mode a where
  ToHsRet Caller a = Promised (ServiceArg a) a
  ToHsRet Defn a = JSObject (ServiceArg a)

type ToWorkerFun a = FromFunSig Defn a

type ToCallerFun a = FromFunSig Caller a

type family FromFunSig mode fn where
  FromFunSig mode (Return a) = IO (ToHsRet mode a)
  FromFunSig mode (a :~> b) = a -> FromFunSig mode b

type family ToJSFunSig fn = js where
  ToJSFunSig (Return a) = ReturnJS (ServiceArg a)
  ToJSFunSig (a :~> b) = ServiceArg a :~>> ToJSFunSig b

type IsServiceFunSig :: FunSig -> Constraint
class IsServiceFunSig fn where
  encodeJSFun# :: Proxy# fn -> FromFunSig Defn fn -> IO (JSFun (ToJSFunSig fn))
  decodeFun# :: Proxy# fn -> JSFun (ToJSFunSig fn) -> FromFunSig Caller fn
  joinHsFun# :: Proxy# fn -> Proxy# mode -> IO (FromFunSig mode fn) -> FromFunSig mode fn

joinHsFun :: forall mode fn -> (IsServiceFunSig fn) => IO (FromFunSig mode fn) -> FromFunSig mode fn
{-# INLINE joinHsFun #-}
joinHsFun mode fn = joinHsFun# (proxy# @fn) (proxy# @mode)

instance (IsServiceArg a) => IsServiceFunSig (Return a) where
  encodeJSFun# _ = coerce
  {-# INLINE encodeJSFun# #-}
  decodeFun# _ =
    pure
      . Promised
        ( either (throwIO . FunResultDecodeFailure) pure
            <=< parseServiceArg
        )
      . unsafeCast @_ @(PromiseClass (ServiceArg a))
  {-# INLINE decodeFun# #-}
  joinHsFun# _ _ = join
  {-# INLINE joinHsFun# #-}

instance (IsServiceArg a, IsServiceFunSig bs) => IsServiceFunSig (a :~> bs) where
  encodeJSFun# _ f = js_ffi_fun_arrow @(ServiceArg a) @(ToJSFunSig bs) $ \x -> do
    xjs <- either (throwIO . FunArgDecodeFailure) pure =<< parseServiceArg @a x
    encodeJSFun# (proxy# @bs) (f xjs)

  decodeFun# _ f x = joinHsFun Caller bs do
    xjs <- encodeServiceArg x
    pure $ decodeFun# (proxy# @bs) $ js_ffi_app_fun f xjs

  joinHsFun# _ (_ :: Proxy# mode) f x = joinHsFun mode bs $ ($ x) <$> f
  {-# INLINE joinHsFun# #-}

type data ServiceClass :: [(Symbol, FunSig)] -> Prototype

type Service fs = JSObject (ServiceClass fs)

type data JSFunClass :: JSFunSig -> Prototype

type JSFun fn = JSObject (JSFunClass fn)

{- | N.B. Generic class involves curry/uncurrying on each function argument.
If you have a function with many arguments, this can lead to performance degression.
In such cases, we recommend to write the instance manually.
-}
class ToService a where
  type Signature a :: [(Symbol, FunSig)]
  type Signature a = GSignature (Rep a)

  toService :: a -> IO (JSObject (ServiceClass (Signature a)))
  default toService ::
    (GenericToService a) =>
    a ->
    IO (JSObject (ServiceClass (Signature a)))
  toService = genericToService

type GenericToService a =
  ( Generic a
  , GToService (Rep a)
  , Signature a ~ GSignature (Rep a)
  )

genericToService ::
  (GenericToService a) =>
  a ->
  IO (JSObject (ServiceClass (GSignature (Rep a))))
genericToService a = do
  sink <- js_new_service_sink
  gwriteField sink $ from a
  pure sink.runServiceSink

class GToService f where
  type GSignature f :: [(Symbol, FunSig)]
  gwriteField :: ServiceSink (GSignature f) -> f a -> IO ()

instance GToService U1 where
  type GSignature U1 = '[]
  gwriteField _ _ = pure ()

castSink :: forall fs fs0. ServiceSink fs0 -> ServiceSink fs
castSink = ServiceSink . unsafeCast . (.runServiceSink)

type family ToFunSig a where
  ToFunSig (IO a) = Return a
  ToFunSig (a -> b) = a ~> ToFunSig b

class (IsServiceFunSig (ToFunSig a)) => IsWorkerFun a where
  toWorkerFun :: a -> ToWorkerFun (ToFunSig a)

instance (IsServiceArg a) => IsWorkerFun (IO a) where
  toWorkerFun = (encodeServiceArg =<<)
  {-# INLINE toWorkerFun #-}

instance (IsServiceArg a, IsWorkerFun b) => IsWorkerFun (a -> b) where
  toWorkerFun f = \x -> toWorkerFun (f x)
  {-# INLINE toWorkerFun #-}

instance
  ( KnownSymbol l
  , IsWorkerFun c
  ) =>
  GToService (S1 ('MetaSel ('Just l) _pk _st _d) (K1 i c))
  where
  type GSignature (S1 ('MetaSel ('Just l) _pk _st _d) (K1 i c)) = '[ '(l, ToFunSig c)]
  gwriteField sink (M1 (K1 x)) = js_set_handler sink (toJSString $ symbolVal' @l proxy#) =<< encodeJSFun# (proxy# @(ToFunSig c)) (toWorkerFun x)

instance (GToService f) => GToService (D1 i f) where
  type GSignature (D1 i f) = GSignature f
  gwriteField sink (M1 x) = gwriteField sink x

instance (GToService f) => GToService (C1 i f) where
  type GSignature (C1 i f) = GSignature f
  gwriteField sink (M1 x) = gwriteField sink x

instance (GToService f, GToService g) => GToService (f :*: g) where
  type GSignature (f :*: g) = GSignature f ++ GSignature g
  gwriteField sink (f :*: g) = gwriteField (castSink @(GSignature f) sink) f >> gwriteField (castSink @(GSignature g) sink) g

instance (Unsatisfiable ('Text "Sum type cannot be marshaled into a service binding")) => GToService (f :+: g) where
  type GSignature (f :+: g) = TypeError ('Text "Sum type cannot be marshaled into a service binding")

type family ls ++ rs where
  '[] ++ rs = rs
  (l ': ls) ++ rs = l ': (ls ++ rs)

class IsServiceArg a where
  type ServiceArg a :: Prototype
  encodeServiceArg :: a -> IO (JSObject (ServiceArg a))
  parseServiceArg :: JSObject (ServiceArg a) -> IO (Either String a)

instance IsServiceArg JSVal where
  type ServiceArg JSVal = AnyClass
  encodeServiceArg = pure . unJSObject
  {-# INLINE encodeServiceArg #-}
  parseServiceArg = pure . Right . unsafeAsObject
  {-# INLINE parseServiceArg #-}

instance (FromJSON a, ToJSON a) => IsServiceArg (ViaJSON a) where
  type ServiceArg (ViaJSON a) = JSONClass
  encodeServiceArg = encodeJSON . runViaJSON
  {-# INLINE encodeServiceArg #-}
  parseServiceArg = fmap (fmap ViaJSON) . eitherDecodeJSON
  {-# INLINE parseServiceArg #-}

instance IsServiceArg (JSObject a) where
  type ServiceArg (JSObject a) = a
  encodeServiceArg = pure
  {-# INLINE encodeServiceArg #-}
  parseServiceArg = pure . Right
  {-# INLINE parseServiceArg #-}

newtype ViaJSPrim a = ViaJSPrim {runViaJSPrim :: a}

instance (JSPrimitive a) => IsServiceArg (ViaJSPrim a) where
  type ServiceArg (ViaJSPrim a) = JSPrimClass a
  encodeServiceArg = pure . toJSPrim . (.runViaJSPrim)
  {-# INLINE encodeServiceArg #-}
  parseServiceArg = pure . Right . ViaJSPrim . fromJSPrim
  {-# INLINE parseServiceArg #-}

instance (IsServiceArg a) => IsServiceArg (Maybe a) where
  type ServiceArg (Maybe a) = NullableClass (ServiceArg a)
  encodeServiceArg = maybe (pure none) (fmap nonNull . encodeServiceArg)
  {-# INLINE encodeServiceArg #-}
  parseServiceArg = nullable (pure $ Right Nothing) (fmap (fmap Just) . parseServiceArg)
  {-# INLINE parseServiceArg #-}

instance (IsServiceArg a) => IsServiceArg (V.Vector a) where
  type ServiceArg (V.Vector a) = SequenceClass (ServiceArg a)
  encodeServiceArg = fmap toSequence . mapM encodeServiceArg
  {-# INLINE encodeServiceArg #-}
  parseServiceArg = runExceptT . mapM (ExceptT . parseServiceArg) <=< toVector
  {-# INLINE parseServiceArg #-}

instance (IsServiceArg a) => IsServiceArg [a] where
  type ServiceArg [a] = SequenceClass (ServiceArg a)
  encodeServiceArg = encodeServiceArg . V.fromList
  {-# INLINE encodeServiceArg #-}
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

newtype ServiceSink fs = ServiceSink {runServiceSink :: Service fs}

foreign import javascript unsafe "$1.$2 = $3"
  js_set_handler :: ServiceSink fs -> JSString -> JSFun e -> IO ()

foreign import javascript unsafe "{}"
  js_new_service_sink :: IO (ServiceSink fs)

foreign import javascript "wrapper"
  js_ffi_fun_arrow :: (JSObject f -> IO (JSFun fs)) -> IO (JSFun (f :~>> fs))

foreign import javascript unsafe "$1($2)"
  js_ffi_app_fun :: JSFun (f :~>> fs) -> JSObject f -> JSFun fs

foreign import javascript unsafe "$1[$2]"
  js_get_fun :: Service fs -> JSString -> IO (JSFun f)
