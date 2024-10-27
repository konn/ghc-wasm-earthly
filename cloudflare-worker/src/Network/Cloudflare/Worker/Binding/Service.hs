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
module Network.Cloudflare.Worker.Binding.Service {- (
                                                   Fun (..),
                                                   ServiceBindingException(..),
                                                   ServiceClass,
                                                   Service,
                                                   call,
                                                   ToServiceBinding (..),
                                                   genericToServiceBinding,
                                                   GenericToServiceBinding,
                                                   ToJSFun (..),
                                                   KnownJSFunSig (..),
                                                   FromFun,
                                                   IsServiceArg (..),
                                                   ViaJSPrim (..),
                                                   ViaJSON (..),
                                                 )  -} where

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

type family FromFunSig mode fn where
  FromFunSig mode (Return a) = IO (ToHsRet mode a)
  FromFunSig mode (a :~> b) = a -> FromFunSig mode b

type family ToJSFunSig fn = js where
  ToJSFunSig (Return a) = ReturnJS (ServiceArg a)
  ToJSFunSig (a :~> b) = ServiceArg a :~>> ToJSFunSig b

type KnownFunSig :: FunSig -> Constraint
class (KnownJSFunSig (ToJSFunSig fn)) => KnownFunSig fn where
  encodeJSFun# :: Proxy# fn -> FromFunSig Defn fn -> ToHaskellFunSig Defn (ToJSFunSig fn)
  decodeFun# :: Proxy# fn -> ToHaskellFunSig Caller (ToJSFunSig fn) -> FromFunSig Caller fn
  joinHsFun# :: Proxy# fn -> Proxy# mode -> IO (FromFunSig mode fn) -> FromFunSig mode fn

joinHsFun :: forall mode fn -> (KnownFunSig fn) => IO (FromFunSig mode fn) -> FromFunSig mode fn
{-# INLINE joinHsFun #-}
joinHsFun mode fn = joinHsFun# (proxy# @fn) (proxy# @mode)

instance (IsServiceArg a) => KnownFunSig (Return a) where
  encodeJSFun# _ = (encodeServiceArg =<<)
  {-# INLINE encodeJSFun# #-}
  decodeFun# _ =
    fmap $
      Promised $
        either (throwIO . FunResultDecodeFailure) pure
          <=< parseServiceArg @a
  {-# INLINE decodeFun# #-}
  joinHsFun# _ _ = join
  {-# INLINE joinHsFun# #-}

instance (IsServiceArg a, KnownFunSig bs) => KnownFunSig (a :~> bs) where
  encodeJSFun# _ f x = joinJsFun Defn (ToJSFunSig bs) do
    xhs <- either (throwIO . FunArgDecodeFailure) pure =<< parseServiceArg @a x
    pure $ encodeJSFun# (proxy# @bs) $! f xhs

  decodeFun# _ f x = joinHsFun Caller bs do
    xjs <- encodeServiceArg x
    pure $ decodeFun# (proxy# @bs) (f xjs)

  joinHsFun# _ (_ :: Proxy# mode) f x = joinHsFun mode bs $ ($ x) <$> f
  {-# INLINE joinHsFun# #-}

type family JSRetFor mode a where
  JSRetFor Caller a = PromiseClass a
  JSRetFor Defn a = a

type family ToHaskellFunSig mode fn where
  ToHaskellFunSig mode (ReturnJS a) = IO (JSObject (JSRetFor mode a))
  ToHaskellFunSig mode (a :~>> b) = JSObject a -> ToHaskellFunSig mode b

joinJsFun :: forall mode fn -> (KnownJSFunSig fn) => IO (ToHaskellFunSig mode fn) -> ToHaskellFunSig mode fn
{-# INLINE joinJsFun #-}
joinJsFun mode fn = joinJsFun# (proxy# @fn) (proxy# @mode)

type KnownJSFunSig :: JSFunSig -> Constraint
class KnownJSFunSig fn where
  marshalJSFun :: ToHaskellFunSig Defn fn -> IO (JSObject (JSFunClass fn))
  unmarshalFun :: JSFun fn -> ToHaskellFunSig Caller fn
  joinJsFun# :: Proxy# fn -> Proxy# mode -> IO (ToHaskellFunSig mode fn) -> ToHaskellFunSig mode fn

instance KnownJSFunSig (ReturnJS f) where
  marshalJSFun = coerce
  unmarshalFun = js_return_fun
  joinJsFun# _ _ = join

instance (KnownJSFunSig fs) => KnownJSFunSig (f :~>> fs) where
  marshalJSFun f = js_ffi_fun_arrow @f @fs $ marshalJSFun . f
  unmarshalFun jsf x = unmarshalFun $ js_ffi_app_fun jsf x
  joinJsFun# _ (_ :: Proxy# mode) f x = joinJsFun mode fs $ ($ x) <$> f

type data ServiceClass :: [(Symbol, FunSig)] -> Prototype

type Service fs = JSObject (ServiceClass fs)

type data JSFunClass :: JSFunSig -> Prototype

type JSFun fn = JSObject (JSFunClass fn)

{- | N.B. Generic class involves curry/uncurrying on each function argument.
If you have a function with many arguments, this can lead to performance degression.
In such cases, we recommend to write the instance manually.
-}
class ToServiceBinding a where
  type Signature a :: [(Symbol, FunSig)]

  -- type Signature a = GSignature (Rep a)
  toServiceBinding :: a -> IO (JSObject (ServiceClass (Signature a)))

{-
  default toServiceBinding ::
    (GenericToServiceBinding a) =>
    a ->
    IO (JSObject (ServiceClass (Signature a)))
  toServiceBinding = genericToServiceBinding
-}

{-
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

instance GToServiceBinding U1 where
  type GSignature U1 = '[]
  gwriteField _ _ = pure ()

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
class (KnownJSFunSig (JSFunOf a)) => ToJSFun a where
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
 -}

class IsServiceArg a where
  type ServiceArg a :: Prototype
  encodeServiceArg :: a -> IO (JSObject (ServiceArg a))
  parseServiceArg :: JSObject (ServiceArg a) -> IO (Either String a)

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

foreign import javascript unsafe "$1"
  js_return_fun :: JSFun (ReturnJS f) -> IO (Promise f)

foreign import javascript unsafe "$1[$2]"
  js_cal_raw :: Service fs -> JSString -> IO (JSFun f)
