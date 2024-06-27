{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Steward.Types where

import Data.Aeson (FromJSON)
import Data.Aeson qualified as J
import Data.Bifunctor qualified as Bi
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Kind
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits
import Network.HTTP.Types (Query, RequestHeaders, ResponseHeaders, Status (..), StdMethod (..))
import Streaming.ByteString.Char8 qualified as Q

class MonadHandler m

class MonadClient m

data ParseResult a = NoMatch | Failed String | Parsed a
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)

instance Applicative ParseResult where
  pure = Parsed
  Failed e <*> _ = Failed e
  NoMatch <*> _ = NoMatch
  Parsed f <*> a = f <$> a

class FromPathPieces a where
  parsePathPieces :: [T.Text] -> ParseResult (a, [T.Text])

class ToPathPieces a where
  toPathPieces :: a -> [T.Text] -> [T.Text]

instance FromPathPieces T.Text where
  parsePathPieces [] = NoMatch
  parsePathPieces (x : xs) = Parsed (x, xs)

instance ToPathPieces T.Text where
  toPathPieces = (:)

type Paths :: [Symbol] -> Type -> Type
newtype Paths paths a = Paths {unPaths :: a}

type KnownSymbols :: [Symbol] -> Constraint
class KnownSymbols ss where
  symbolsVal' :: Proxy# ss -> [T.Text]

instance KnownSymbols '[] where
  symbolsVal' _ = []

instance (KnownSymbol s, KnownSymbols ss) => KnownSymbols (s : ss) where
  symbolsVal' _ = T.pack (symbolVal' (proxy# @s)) : symbolsVal' (proxy# @ss)

symbolsVal :: forall ss -> (KnownSymbols ss) => [T.Text]
symbolsVal ss = symbolsVal' (proxy# @ss)

instance
  (KnownSymbols paths, FromPathPieces a) =>
  FromPathPieces (Paths paths a)
  where
  parsePathPieces xs =
    let syms = symbolsVal paths
     in case List.stripPrefix syms xs of
          Just rest -> Bi.first Paths <$> parsePathPieces rest
          Nothing -> NoMatch

instance (KnownSymbols paths, ToPathPieces a) => ToPathPieces (Paths paths a) where
  toPathPieces (Paths a) = (symbolsVal paths <>) . toPathPieces a

newtype Captures captured a = Captures {getCaptures :: captured -> a}

infixr 4 />

type (/>) :: k -> Type -> Type
type data paths /> a

infixr 0 ~>

type family xs ~> a where
  '[] ~> t = t
  (x : xs) ~> t = x -> (xs ~> t)

data StewardRequest = StewardRequest
  { method :: !StdMethod
  , headers :: !RequestHeaders
  , secure :: !Bool
  , host :: !BS.ByteString
  , port :: !BS.ByteString
  , rawPathInfo :: !BS.ByteString
  , rawQueryString :: !BS.ByteString
  , body :: LBS.ByteString
  , pathInfo :: ![T.Text]
  , queryString :: Query
  }
  deriving (Generic)

data StewardResponse = StewardResponse
  { stauts :: !Status
  , headers :: !ResponseHeaders
  , body :: !(Q.ByteStream IO ())
  }
  deriving (Generic)

type ServerApp m = m StewardResponse

class Routable m a where
  type RouteRet m a :: Type
  type RouteArgs m a :: [Type]
  matchRoute' ::
    Proxy# a ->
    StewardRequest ->
    [T.Text] ->
    ParseResult ((RouteArgs m a ~> m (RouteRet m a)) -> ServerApp m)

instance (KnownSymbol s, Routable m t) => Routable m (s /> t) where
  type RouteArgs m (s /> t) = RouteArgs m t
  type RouteRet m (s /> t) = RouteRet m t
  matchRoute' _ req (x : xs)
    | x == T.pack (symbolVal' @s proxy#) =
        matchRoute' (proxy# @t) req xs
  matchRoute' _ _ _ = NoMatch

instance (KnownSymbols ss, Routable m t) => Routable m (ss /> t) where
  type RouteArgs m (ss /> t) = RouteArgs m t
  type RouteRet m (ss /> t) = RouteRet m t

  matchRoute' _ req xs
    | Just ys <- List.stripPrefix (symbolsVal ss) xs =
        matchRoute' (proxy# @t) req ys
  matchRoute' _ _ _ = NoMatch

instance (FromPathPieces x, Routable m t) => Routable m (x /> t) where
  type RouteArgs m (x /> t) = x : RouteArgs m t
  type RouteRet m (x /> t) = RouteRet m t

  matchRoute' _ req xs = case parsePathPieces @x xs of
    NoMatch -> NoMatch
    Failed e -> Failed e
    Parsed (a, ys) -> case matchRoute' (proxy# @t) req ys of
      NoMatch -> NoMatch
      Failed e -> Failed e
      Parsed f -> Parsed \g -> f (g a)

type data Header_ = Header Symbol

instance (KnownSymbol s, Routable m t) => Routable m (Header s /> t) where
  type RouteArgs m (Header s /> t) = Maybe BS.ByteString ': RouteArgs m t
  type RouteRet m (Header s /> t) = RouteRet m t

  matchRoute' _ req xs =
    case matchRoute' (proxy# @t) req xs of
      NoMatch -> NoMatch
      Failed e -> Failed e
      Parsed f -> Parsed \g ->
        f $
          g $
            lookup
              (CI.mk $ TE.encodeUtf8 $ T.pack $ symbolVal' @s proxy#)
              req.headers

type JSONBody_ :: Type
type data JSONBody_ = JSONBody Type

instance (FromJSON a, Routable m t) => Routable m (JSONBody a /> t) where
  type RouteArgs m (JSONBody a /> t) = a ': RouteArgs m t
  type RouteRet m (JSONBody a /> t) = RouteRet m t

  matchRoute' _ req xs =
    case matchRoute' (proxy# @t) req xs of
      NoMatch -> NoMatch
      Failed e -> Failed e
      Parsed f -> case J.decode req.body of
        Nothing -> Failed "Failed to decode JSON body"
        Just a -> Parsed \g -> f (g a)

type RawBody_ :: Type
type data RawBody_ = RawBody

instance (Routable m t) => Routable m (RawBody /> t) where
  type RouteArgs m (RawBody /> t) = LBS.ByteString ': RouteArgs m t
  type RouteRet m (RawBody /> t) = RouteRet m t

  matchRoute' _ req xs =
    case matchRoute' (proxy# @t) req xs of
      NoMatch -> NoMatch
      Failed e -> Failed e
      Parsed f -> Parsed \g -> f (g req.body)

type RawRequest_ :: Type
type data RawRequest_ = RawRequest

instance (Routable m t) => Routable m (RawRequest /> t) where
  type RouteArgs m (RawRequest /> t) = StewardRequest ': RouteArgs m t
  type RouteRet m (RawRequest /> t) = RouteRet m t

  matchRoute' _ req xs =
    case matchRoute' (proxy# @t) req xs of
      NoMatch -> NoMatch
      Failed e -> Failed e
      Parsed f -> Parsed \g -> f (g req)

type Verb :: StdMethod -> Nat -> ResponseType -> Type
type data Verb method status responseType

instance
  (Monad m, KnownMethod meth, KnownNat status, IsResponseType responseType) =>
  Routable m (Verb meth status responseType)
  where
  type RouteArgs m (Verb meth status responseType) = '[]
  type RouteRet m (Verb meth status responseType) = ResponseContent responseType
  matchRoute' _ req []
    | req.method == methodVal meth =
        Parsed \get -> do
          body <- encodeResponse responseType <$> get
          pure $
            StewardResponse
              { stauts =
                  Status
                    { statusMessage = mempty
                    , statusCode = fromIntegral $ natVal' @status proxy#
                    }
              , headers = []
              , body = Q.fromLazy body
              }
    | otherwise = NoMatch
  matchRoute' _ _ _ = NoMatch

type ContentType = Type

data ResponseType = JSON Type | PlainText | NoContent
  deriving (Generic)

type IsResponseType :: ResponseType -> Constraint
class IsResponseType rt where
  type ResponseContent rt :: a
  encodeResponse' :: Proxy# rt -> ResponseContent rt -> LBS.ByteString

instance (J.ToJSON a) => IsResponseType (JSON a) where
  type ResponseContent (JSON a) = a
  encodeResponse' _ = J.encode

instance IsResponseType PlainText where
  type ResponseContent PlainText = LT.Text
  encodeResponse' _ = LTE.encodeUtf8

instance IsResponseType NoContent where
  type ResponseContent NoContent = ()
  encodeResponse' _ _ = LBS.empty

encodeResponse :: forall rt -> (IsResponseType rt) => ResponseContent rt -> LBS.ByteString
encodeResponse rt = encodeResponse' (proxy# @rt)

type Get = Verb GET 200

type Post = Verb POST 200

type Put = Verb PUT 200

type Delete = Verb DELETE 200

type Patch = Verb PATCH 200

class KnownMethod (method :: StdMethod) where
  methodVal' :: Proxy# method -> StdMethod

instance KnownMethod 'GET where
  methodVal' _ = GET

instance KnownMethod 'POST where
  methodVal' _ = POST

instance KnownMethod 'PUT where
  methodVal' _ = PUT

instance KnownMethod 'HEAD where
  methodVal' _ = HEAD

instance KnownMethod 'DELETE where
  methodVal' _ = DELETE

instance KnownMethod 'TRACE where
  methodVal' _ = TRACE

instance KnownMethod 'OPTIONS where
  methodVal' _ = OPTIONS

instance KnownMethod 'CONNECT where
  methodVal' _ = CONNECT

instance KnownMethod 'PATCH where
  methodVal' _ = PATCH

methodVal :: forall method -> (KnownMethod method) => StdMethod
methodVal method = methodVal' (proxy# @method)

class KnownMethods (methods :: [StdMethod]) where
  methodsVal' :: Proxy# methods -> [StdMethod]

instance KnownMethods '[] where
  methodsVal' _ = []

instance
  (KnownMethod m, KnownMethods methods) =>
  KnownMethods (m : methods)
  where
  methodsVal' _ = methodVal m : methodsVal' (proxy# @methods)

methodsVal :: forall methods -> (KnownMethods methods) => [StdMethod]
methodsVal methods = methodsVal' (proxy# @methods)
