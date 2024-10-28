{-# LANGUAGE AllowAmbiguousTypes #-}
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
  ServiceM,
  getBinding,
  getEnv,
  getRawEnv,
  getSecret,
  getFetchContext,
  waitUntil,
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
) where

import Control.Arrow ((>>>))
import Control.Exception.Safe (Exception, MonadCatch, MonadMask, MonadThrow, throwIO)
import Control.Monad (join, (<=<))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Coerce (coerce)
import Data.Int
import Data.Kind (Constraint, Type)
import Data.Text qualified as T
import Data.Type.Bool (If, type (&&))
import Data.Vector qualified as V
import Data.Word
import Effectful (MonadUnliftIO)
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics
import GHC.Records
import GHC.TypeError
import GHC.TypeLits
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Prim
import GHC.Wasm.Web.JSON
import Language.WASM.JSVal.Convert
import Network.Cloudflare.Worker.Binding (BindingsClass, ListMember)
import Network.Cloudflare.Worker.Binding qualified as B
import Network.Cloudflare.Worker.Handler.Fetch (FetchContext)

type data FunSig = Return Type | Type :~> FunSig

type data JSFunSig = ReturnJS Prototype | Prototype :~>> JSFunSig

type ServiceM :: Prototype -> [(Symbol, FunSig)] -> Type -> Type
newtype ServiceM e bs a = ServiceM {runServiceM :: IO a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

getBinding ::
  forall es ss bs fs.
  forall l ->
  (KnownSymbol l, Member l bs) =>
  ServiceM (BindingsClass es ss bs) fs (JSObject (Lookup' l bs))
getBinding l = B.getBinding l <$> js_get_env

getSecret ::
  forall es ss bs fs.
  forall l ->
  (KnownSymbol l, ListMember l ss) =>
  ServiceM (BindingsClass es ss bs) fs T.Text
getSecret l = B.getSecret l <$> js_get_env

getEnv ::
  forall es ss bs fs a.
  forall l ->
  (KnownSymbol l, ListMember l es, FromJSON a) =>
  ServiceM (BindingsClass es ss bs) fs a
getEnv l =
  getRawEnv l
    >>= ( J.fromJSON >>> \case
            J.Error e -> throwIO $ FunResultDecodeFailure e
            J.Success x -> pure x
        )

getRawEnv ::
  forall es ss bs fs.
  forall l ->
  (KnownSymbol l, ListMember l es) =>
  ServiceM (BindingsClass es ss bs) fs J.Value
getRawEnv l =
  B.getEnv l <$> js_get_env

call ::
  forall bs x.
  forall l ->
  (KnownSymbol l, Member l bs, Lookup l bs ~ 'Just x, IsServiceFunSig x) =>
  Service bs ->
  ToCallerFun x
call l srv =
  joinHsFun x $
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

data FunMode = Caller | Defn Prototype [(Symbol, FunSig)]

data ServiceBindingException
  = FunResultDecodeFailure !String
  | FunArgDecodeFailure !String
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

type ToWorkerFun e fs a = FromFunSig (Defn e fs) a

type ToCallerFun a = FromFunSig Caller a

type family FromFunSig mode fn where
  FromFunSig (Defn e fs) (Return a) = ServiceM e fs a
  FromFunSig Caller (Return a) = IO (Promised (ServiceArg a) a)
  FromFunSig mode (a :~> b) = a -> FromFunSig mode b

type family ToJSFunSig fn = js where
  ToJSFunSig (Return a) = ReturnJS (ServiceArg a)
  ToJSFunSig (a :~> b) = ServiceArg a :~>> ToJSFunSig b

type IsServiceFunSig :: FunSig -> Constraint
class IsServiceFunSig fn where
  encodeJSFun# ::
    Proxy# fn ->
    Proxy# e ->
    Proxy# fs ->
    FromFunSig (Defn e fs) fn ->
    IO (JSFun (ToJSFunSig fn))
  decodeFun# :: Proxy# fn -> JSFun (ToJSFunSig fn) -> ToCallerFun fn
  joinHsFun# :: Proxy# fn -> IO (FromFunSig Caller fn) -> FromFunSig Caller fn

joinHsFun :: forall fn -> (IsServiceFunSig fn) => IO (ToCallerFun fn) -> ToCallerFun fn
{-# INLINE joinHsFun #-}
joinHsFun fn = joinHsFun# (proxy# @fn)

instance (IsServiceArg a) => IsServiceFunSig (Return a) where
  encodeJSFun# _ _ _ = (coerce . encodeServiceArg @a =<<) . (.runServiceM)
  {-# INLINE encodeJSFun# #-}
  decodeFun# _ =
    pure
      . Promised
        ( either (throwIO . FunResultDecodeFailure) pure
            <=< parseServiceArg
        )
      . unsafeCast @_ @(PromiseClass (ServiceArg a))
  {-# INLINE decodeFun# #-}
  joinHsFun# _ = join
  {-# INLINE joinHsFun# #-}

instance (IsServiceArg a, IsServiceFunSig bs) => IsServiceFunSig (a :~> bs) where
  encodeJSFun# _ e fs f = js_ffi_fun_arrow @(ServiceArg a) @(ToJSFunSig bs) $ \x -> do
    xjs <- either (throwIO . FunArgDecodeFailure) pure =<< parseServiceArg @a x
    encodeJSFun# (proxy# @bs) e fs (f xjs)

  decodeFun# _ f x = joinHsFun bs do
    xjs <- encodeServiceArg x
    pure $ decodeFun# (proxy# @bs) $ js_ffi_app_fun f xjs

  joinHsFun# _ f x = joinHsFun bs $ ($ x) <$> f
  {-# INLINE joinHsFun# #-}

type data ServiceClass :: [(Symbol, FunSig)] -> Prototype

type Service fs = JSObject (ServiceClass fs)

type data JSFunClass :: JSFunSig -> Prototype

type JSFun fn = JSObject (JSFunClass fn)

{- | N.B. Generic class involves curry/uncurrying on each function argument.
If you have a function with many arguments, this can lead to performance degression.
In such cases, we recommend to write the instance manually.
-}
class ToService e a where
  type Signature e a :: [(Symbol, FunSig)]
  type Signature e a = GSignature e (Rep a)

  toService :: a -> IO (Service (Signature e a))
  default toService ::
    (GenericToService e a) =>
    a ->
    IO (JSObject (ServiceClass (Signature e a)))
  toService = genericToService @e

type GenericToService e a =
  ( Generic a
  , GToService e (GSignature e (Rep a)) (Rep a)
  , Signature e a ~ GSignature e (Rep a)
  )

genericToService ::
  forall e a.
  (GenericToService e a) =>
  a ->
  IO (Service (GSignature e (Rep a)))
genericToService a = do
  sink <- js_new_service_sink
  gwriteField (proxy# @e) (proxy# @(GSignature e (Rep a))) sink $ from a
  pure sink.runServiceSink

type GToService :: Prototype -> [(Symbol, FunSig)] -> (Type -> Type) -> Constraint
class GToService e fs f where
  gwriteField :: Proxy# e -> Proxy# fs -> ServiceSink (GSignature e f) -> f a -> IO ()

instance GToService e fs U1 where
  gwriteField _ _ _ _ = pure ()

castSink :: forall fs fs0. ServiceSink fs0 -> ServiceSink fs
castSink = ServiceSink . unsafeCast . (.runServiceSink)

type family ToFunSig e a where
  ToFunSig _ (IO a) = Return a
  ToFunSig e' (ServiceM e fs a) = Return a
  ToFunSig e (a -> b) = a ~> ToFunSig e b

type family fs ⊆? fs' where
  xs ⊆? xs = 'True
  '[] ⊆? _ = 'True
  (f ': fs) ⊆? fs' = Elem f fs' && fs ⊆? fs'

type family Elem f fs where
  Elem f (f ': _) = 'True
  Elem f (_ ': fs) = Elem f fs
  Elem _ '[] = 'False

type (⊆) :: [a] -> [a] -> Constraint
type family fs ⊆ fs' where
  fs ⊆ fs' = If (fs ⊆? fs') (() :: Constraint) (Unsatisfiable ('Text "fs is not a subset of fs'"))

type family GSignature e f where
  GSignature e (S1 ('MetaSel 'Nothing _a _b _c) _d) = TypeError ('Text "Record field must have a label")
  GSignature e (S1 ('MetaSel ('Just l) _a _b _c) (K1 i c)) = '[ '(l, ToFunSig e c)]
  GSignature e (D1 i f) = GSignature e f
  GSignature e (C1 i f) = GSignature e f
  GSignature e (f :*: g) = GSignature e f ++ GSignature e g
  GSignature e (f :+: g) = TypeError ('Text "Sum type cannot be marshaled into a service binding")

class (IsServiceFunSig (ToFunSig e a)) => IsWorkerFun e fs a where
  toWorkerFun :: Proxy# e -> Proxy# fs -> a -> ToWorkerFun e fs (ToFunSig e a)

instance
  (IsServiceArg a) =>
  IsWorkerFun e fs (IO a)
  where
  toWorkerFun _ _ = ServiceM
  {-# INLINE toWorkerFun #-}

instance
  (IsServiceArg a, e ~ e', fs' ⊆ fs) =>
  IsWorkerFun e fs (ServiceM e' fs' a)
  where
  toWorkerFun _ _ = coerce
  {-# INLINE toWorkerFun #-}

instance (IsServiceArg a, IsWorkerFun e fs b) => IsWorkerFun e fs (a -> b) where
  toWorkerFun e fs f = \x -> toWorkerFun e fs (f x)
  {-# INLINE toWorkerFun #-}

instance
  ( Unsatisfiable ('Text "Record field must have a label")
  ) =>
  GToService e fs (S1 ('MetaSel 'Nothing _pk _st _d) (K1 i c))

instance
  ( KnownSymbol l
  , IsWorkerFun e fs c
  ) =>
  GToService e fs (S1 ('MetaSel ('Just l) _pk _st _d) (K1 i c))
  where
  gwriteField e fs sink (M1 (K1 x)) = js_set_handler sink (toJSString $ symbolVal' @l proxy#) =<< encodeJSFun# (proxy# @(ToFunSig e c)) e fs (toWorkerFun (proxy# @e) (proxy# @fs) x)

instance (GToService e fs f) => GToService e fs (D1 i f) where
  gwriteField e fs sink (M1 x) = gwriteField e fs sink x

instance (GToService e fs f) => GToService e fs (C1 i f) where
  gwriteField e fs sink (M1 x) = gwriteField e fs sink x

instance (GToService e fs f, GToService e fs g) => GToService e fs (f :*: g) where
  gwriteField e fs sink (f :*: g) =
    gwriteField e fs (castSink @(GSignature e f) sink) f
      >> gwriteField e fs (castSink @(GSignature e g) sink) g

instance (Unsatisfiable ('Text "Sum type cannot be marshaled into a service binding")) => GToService e fs (f :+: g)

type family ls ++ rs where
  '[] ++ rs = rs
  (l ': ls) ++ rs = l ': (ls ++ rs)

class IsServiceArg a where
  type ServiceArg a :: Prototype
  encodeServiceArg :: a -> IO (JSObject (ServiceArg a))
  parseServiceArg :: JSObject (ServiceArg a) -> IO (Either String a)

instance IsServiceArg JSVal where
  type ServiceArg JSVal = AnyClass
  encodeServiceArg = pure . unsafeAsObject
  {-# INLINE encodeServiceArg #-}
  parseServiceArg = pure . Right . unJSObject
  {-# INLINE parseServiceArg #-}

instance IsServiceArg JSString where
  type ServiceArg JSString = USVStringClass
  encodeServiceArg = pure . coerce
  {-# INLINE encodeServiceArg #-}
  parseServiceArg = pure . Right . coerce
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

foreign import javascript unsafe "this.env"
  js_get_env :: ServiceM e fs (JSObject e)

foreign import javascript unsafe "this.ctx"
  getFetchContext :: ServiceM e fs FetchContext

foreign import javascript unsafe "this.ctx.waitUntil($1)"
  waitUntil :: Promise a -> ServiceM e fs ()

data Add = Add
  { add :: Int -> Int -> IO Int
  , sub :: Int -> Int -> ServiceM AnyClass ('[ '("add", Int ~> Int ~> Return Int)]) Int
  }
  deriving (Generic, ToService AnyClass)
