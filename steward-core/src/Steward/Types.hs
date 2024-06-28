{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Steward.Types (
  StewardRequest (..),
  PartialRequest (..),
  PreRoutable (..),
  HasHandler (..),
  runHandlers,
  Routable (..),
  MonadHandler (..),
  MonadClient (..),
  KnownSymbols (),
  KnownMethod (),
  type (/>),
  type (:::) (..),
  type (~>),
  FromPathPieces (..),
  ToPathPieces (..),
  ParseResult (..),
  Verb,
  Get,
  Post,
  Put,
  Delete,
  Patch,
  StdMethod (..),
  Modifier (..),
  ResponseType (..),
  IsResponseType (..),
  Client,
  Handler,
) where

import Control.Lens ((%~), (&), (.~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Generics.Labels ()
import Data.Kind
import Data.List qualified as List
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics
import GHC.Generics qualified as Generics
import GHC.TypeLits
import Network.HTTP.Types (Query, RequestHeaders, ResponseHeaders, Status (..), StdMethod (..), status404, status500)
import Streaming.ByteString.Char8 qualified as Q

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
  toPathPieces :: a -> [T.Text]

instance FromPathPieces T.Text where
  parsePathPieces [] = NoMatch
  parsePathPieces (x : xs) = Parsed (x, xs)

instance ToPathPieces T.Text where
  toPathPieces = pure

type KnownSymbols :: [Symbol] -> Constraint
class KnownSymbols ss where
  symbolsVal' :: Proxy# ss -> [T.Text]

instance KnownSymbols '[] where
  symbolsVal' _ = []

instance (KnownSymbol s, KnownSymbols ss) => KnownSymbols (s : ss) where
  symbolsVal' _ = T.pack (symbolVal' (proxy# @s)) : symbolsVal' (proxy# @ss)

symbolsVal :: forall ss -> (KnownSymbols ss) => [T.Text]
symbolsVal ss = symbolsVal' (proxy# @ss)

infixr 4 />

type (/>) :: k -> Type -> Type
type data paths /> a

infixr 0 ~>

type family xs ~> a where
  '[] ~> t = t
  (x : xs) ~> t = x -> (xs ~> t)

data PartialRequest = PartialRequest
  { pathInfo :: ![T.Text]
  , queryString :: !Query
  , method :: !StdMethod
  , body :: !LBS.ByteString
  , headers :: !RequestHeaders
  }
  deriving (Generic)

data StewardRequest = StewardRequest
  { method :: !StdMethod
  , headers :: !RequestHeaders
  , secure :: !Bool
  , host :: !BS.ByteString
  , port :: !BS.ByteString
  , rawPathInfo :: !BS.ByteString
  , rawQueryString :: !BS.ByteString
  , body :: !LBS.ByteString
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

class PreRoutable a where
  type RouteArgs a :: [Type]
  buildRequest' :: Proxy# a -> HList (RouteArgs a) -> PartialRequest

class (PreRoutable a) => Routable m a where
  type ResponseSeed m a :: Type
  matchRoute' ::
    Proxy# a ->
    StewardRequest ->
    [T.Text] ->
    ParseResult ((RouteArgs a ~> m (ResponseSeed m a)) -> m StewardResponse)

matchRoute ::
  forall a ->
  (Routable m a) =>
  StewardRequest ->
  [T.Text] ->
  ParseResult ((RouteArgs a ~> m (ResponseSeed m a)) -> m StewardResponse)
matchRoute a = matchRoute' (proxy# @a)

instance (KnownSymbol s, PreRoutable t) => PreRoutable (s /> t) where
  type RouteArgs (s /> t) = RouteArgs t
  buildRequest' _ =
    (#pathInfo %~ (T.pack (symbolVal' @s proxy#) :))
      . buildRequest' (proxy# @t)

instance (KnownSymbol s, Routable m t) => Routable m (s /> t) where
  type ResponseSeed m (s /> t) = ResponseSeed m t
  matchRoute' _ req (x : xs)
    | x == T.pack (symbolVal' @s proxy#) =
        matchRoute' (proxy# @t) req xs
  matchRoute' _ _ _ = NoMatch

instance (KnownSymbols ss, PreRoutable t) => PreRoutable (ss /> t) where
  type RouteArgs (ss /> t) = RouteArgs t
  buildRequest' _ =
    (#pathInfo %~ (symbolsVal ss <>))
      . buildRequest' (proxy# @t)

instance (KnownSymbols ss, Routable m t) => Routable m (ss /> t) where
  type ResponseSeed m (ss /> t) = ResponseSeed m t

  matchRoute' _ req xs
    | Just ys <- List.stripPrefix (symbolsVal ss) xs =
        matchRoute' (proxy# @t) req ys
  matchRoute' _ _ _ = NoMatch

instance
  (ToPathPieces x, PreRoutable t) =>
  PreRoutable (x /> t)
  where
  type RouteArgs (x /> t) = x : RouteArgs t
  buildRequest' _ (x :- xs) =
    buildRequest' (proxy# @t) xs
      & #pathInfo %~ (toPathPieces x <>)

instance
  (ToPathPieces x, FromPathPieces x, Routable m t) =>
  Routable m (x /> t)
  where
  type ResponseSeed m (x /> t) = ResponseSeed m t

  matchRoute' _ req xs = case parsePathPieces @x xs of
    NoMatch -> NoMatch
    Failed e -> Failed e
    Parsed (a, ys) -> case matchRoute' (proxy# @t) req ys of
      NoMatch -> NoMatch
      Failed e -> Failed e
      Parsed f -> Parsed \g -> f (g a)

type data Modifier = Header Symbol | JSONBody Type | RawRequestBody

instance (KnownSymbol s, PreRoutable t) => PreRoutable (Header s /> t) where
  type RouteArgs (Header s /> t) = Maybe BS.ByteString ': RouteArgs t
  buildRequest' _ (mhdr :- xs) =
    let headerName = CI.mk $ TE.encodeUtf8 $ T.pack $ symbolVal' @s proxy#
     in buildRequest' (proxy# @t) xs
          & #headers
            %~ maybe id ((:) . (headerName,)) mhdr . filter ((/= headerName) . fst)

instance (KnownSymbol s, Routable m t) => Routable m (Header s /> t) where
  type ResponseSeed m (Header s /> t) = ResponseSeed m t

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

instance (PreRoutable t) => PreRoutable (RawRequestBody /> t) where
  type RouteArgs (RawRequestBody /> t) = LBS.ByteString ': RouteArgs t
  buildRequest' _ (a :- xs) =
    buildRequest' (proxy# @t) xs & #body .~ a

instance (Routable m t) => Routable m (RawRequestBody /> t) where
  type ResponseSeed m (RawRequestBody /> t) = ResponseSeed m t
  matchRoute' _ req xs =
    case matchRoute' (proxy# @t) req xs of
      NoMatch -> NoMatch
      Failed e -> Failed e
      Parsed f -> Parsed \g -> f (g req.body)

instance (ToJSON a, PreRoutable t) => PreRoutable (JSONBody a /> t) where
  type RouteArgs (JSONBody a /> t) = a ': RouteArgs t
  buildRequest' _ (a :- xs) =
    buildRequest' (proxy# @t) xs
      & #body .~ J.encode a

instance (ToJSON a, FromJSON a, Routable m t) => Routable m (JSONBody a /> t) where
  type ResponseSeed m (JSONBody a /> t) = ResponseSeed m t

  matchRoute' _ req xs =
    case matchRoute' (proxy# @t) req xs of
      NoMatch -> NoMatch
      Failed e -> Failed e
      Parsed f -> case J.decode req.body of
        Nothing -> Failed "Failed to decode JSON body"
        Just a -> Parsed \g -> f (g a)

type Verb :: StdMethod -> Nat -> ResponseType -> Type
type data Verb method status responseType

instance
  (KnownMethod meth) =>
  PreRoutable (Verb meth status responseType)
  where
  type RouteArgs (Verb meth status responseType) = '[]
  buildRequest' _ _ =
    PartialRequest
      { pathInfo = []
      , queryString = mempty
      , method = methodVal meth
      , body = mempty
      , headers = []
      }

instance
  (Monad m, KnownMethod meth, KnownNat status, IsResponseType responseType) =>
  Routable m (Verb meth status responseType)
  where
  type ResponseSeed m (Verb meth status responseType) = ResponseContent responseType
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

infixr 9 :-

data HList xs where
  HNil :: HList '[]
  (:-) :: x -> HList xs -> HList (x : xs)

infixl 0 :::

data family mode ::: api

type Handler :: (Type -> Type) -> Type
data Handler m

newtype instance Handler m ::: api = Handler (RouteArgs api ~> m (ResponseSeed m api))

type Client :: (Type -> Type) -> Type
data Client m

newtype instance Client m ::: api = Client (RouteArgs api ~> m (ResponseSeed m api))

class (Monad m) => MonadClient m where
  request :: PartialRequest -> m StewardResponse

type Application m = StewardRequest -> m StewardResponse

class (Monad m) => MonadHandler m where
  runApp :: Application m -> m ()

class HasHandler m t where
  toApplication :: t (Handler m) -> StewardRequest -> ParseResult (m StewardResponse)
  default toApplication ::
    (GenericHasHandler m (t (Handler m))) =>
    t (Handler m) ->
    StewardRequest ->
    ParseResult (m StewardResponse)
  toApplication = gtoApplication . Generics.from

type GenericHasHandler m a = (Generic a, GHandler m (Rep a))

class GHandler m f where
  gtoApplication :: f (Handler m) -> StewardRequest -> ParseResult (m StewardResponse)

instance (GHandler m f) => GHandler m (M1 i c f) where
  gtoApplication (M1 x) = gtoApplication x

instance {-# OVERLAPPING #-} (Routable m t) => GHandler m (K1 i (Handler m ::: t)) where
  gtoApplication (K1 (Handler f)) r = case matchRoute t r r.pathInfo of
    NoMatch -> NoMatch
    Failed e -> Failed e
    Parsed g -> Parsed $ g f

instance
  {-# OVERLAPPABLE #-}
  (HasHandler m t) =>
  GHandler m (K1 i (t (Handler m)))
  where
  gtoApplication (K1 x) = toApplication x

instance (GHandler m l, GHandler m r) => GHandler m (l :*: r) where
  gtoApplication (l :*: r) req = case gtoApplication l req of
    NoMatch -> gtoApplication r req
    Failed e -> Failed e
    Parsed f -> Parsed f

runHandlers :: (MonadHandler m, HasHandler m t) => t (Handler m) -> m ()
runHandlers hs = runApp \req -> case toApplication hs req of
  NoMatch ->
    pure
      StewardResponse
        { stauts = status404
        , headers = mempty
        , body = "Not Found"
        }
  Failed e ->
    pure
      StewardResponse
        { stauts = status500
        , headers = mempty
        , body = fromString e
        }
  Parsed f -> f
