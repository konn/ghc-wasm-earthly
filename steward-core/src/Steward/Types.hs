{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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
  StewardResponse (..),
  PreRoutable (..),
  HasHandler (..),
  GenericHasHandler,
  runHandlers,
  toApplication,
  HasClient (..),
  GenericHasClient,

  -- * Internal types
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
  ClientException (..),
  FromQueryParams (..),
  GenericFromQueryParams,
  FromQueryParamString (..),
  ToQueryParams (..),
  GenericToQueryParams,
  ToQueryParamString (..),
  DQuery,
  Shown (..),
  Readed (..),
) where

import Control.Exception.Safe (Exception, MonadThrow, throwM)
import Control.Lens ((%~), (&), (.~))
import Control.Monad ((<=<))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Generics.Labels ()
import Data.Int
import Data.Kind
import Data.List qualified as List
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import Data.Word
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics
import GHC.Generics qualified as Generics
import GHC.TypeLits
import Lucid (Html)
import Lucid qualified
import Network.HTTP.Media (MediaType)
import Network.HTTP.Media qualified as Media
import Network.HTTP.Media.MediaType ((//))
import Network.HTTP.Types (Query, QueryItem, RequestHeaders, ResponseHeaders, Status (..), StdMethod (..), hContentType, status404, status500)
import Text.Read (readEither)

data ParseResult a = NoMatch | Failed String | Parsed a
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)

class AppHList xs where
  appHList :: (HList xs -> r) -> xs ~> r

instance AppHList '[] where
  appHList f = f HNil

instance (AppHList xs) => AppHList (x : xs) where
  appHList f x = appHList @xs (f . (x :-))

instance Applicative ParseResult where
  pure = Parsed
  Failed e <*> _ = Failed e
  NoMatch <*> _ = NoMatch
  Parsed f <*> a = f <$> a

class FromQueryParamString a where
  parseQueryParamString :: BS.ByteString -> ParseResult a

instance FromQueryParamString BS.ByteString where
  parseQueryParamString = Parsed

class ToQueryParamString a where
  toQueryParamString :: a -> BS.ByteString

instance FromQueryParamString T.Text where
  parseQueryParamString = Parsed . TE.decodeUtf8

instance ToQueryParamString T.Text where
  toQueryParamString = TE.encodeUtf8

instance FromQueryParamString String where
  parseQueryParamString = Parsed . T.unpack . TE.decodeUtf8

instance ToQueryParamString String where
  toQueryParamString = TE.encodeUtf8 . T.pack

instance FromQueryParamString LT.Text where
  parseQueryParamString = Parsed . LTE.decodeUtf8 . LBS.fromStrict

instance ToQueryParamString LT.Text where
  toQueryParamString = LBS.toStrict . LTE.encodeUtf8

newtype Shown a = Shown {unShown :: a}
  deriving (Generic, Functor, Foldable, Traversable)
  deriving newtype (Show)

instance (Show a) => ToQueryParamString (Shown a) where
  toQueryParamString = BS8.pack . show . (.unShown)

instance (Show a) => ToPathPieces (Shown a) where
  toPathPieces = toPathPieces . show

newtype Readed a = Readed {unReaded :: a}
  deriving (Generic, Functor, Foldable, Traversable)

instance (Read a) => FromQueryParamString (Readed a) where
  parseQueryParamString bs = case readEither (BS8.unpack bs) of
    Right a -> Parsed (Readed a)
    Left e -> Failed e

instance (Read a) => FromPathPieces (Readed a) where
  parsePathPieces [] = NoMatch
  parsePathPieces (x : xs) = case readEither (T.unpack x) of
    Right a -> Parsed (Readed a, xs)
    Left err -> Failed $ "Failed to parse Readed: " <> err

class FromPathPieces a where
  parsePathPieces :: [T.Text] -> ParseResult (a, [T.Text])

class ToPathPieces a where
  toPathPieces :: a -> [T.Text]

instance FromPathPieces T.Text where
  parsePathPieces [] = NoMatch
  parsePathPieces (x : xs) = Parsed (x, xs)

instance FromPathPieces String where
  parsePathPieces [] = NoMatch
  parsePathPieces (x : xs) = Parsed (T.unpack x, xs)

instance ToPathPieces String where
  toPathPieces = pure . T.pack

instance ToPathPieces T.Text where
  toPathPieces = pure

deriving via Shown Integer instance ToPathPieces Integer

deriving via Shown Integer instance ToQueryParamString Integer

deriving via Readed Integer instance FromPathPieces Integer

deriving via Readed Integer instance FromQueryParamString Integer

deriving via Shown Natural instance ToPathPieces Natural

deriving via Shown Natural instance ToQueryParamString Natural

deriving via Readed Natural instance FromPathPieces Natural

deriving via Readed Natural instance FromQueryParamString Natural

deriving via Shown Int instance ToPathPieces Int

deriving via Shown Int instance ToQueryParamString Int

deriving via Readed Int instance FromPathPieces Int

deriving via Readed Int instance FromQueryParamString Int

deriving via Shown Int8 instance ToPathPieces Int8

deriving via Shown Int8 instance ToQueryParamString Int8

deriving via Readed Int8 instance FromPathPieces Int8

deriving via Readed Int8 instance FromQueryParamString Int8

deriving via Shown Int16 instance ToPathPieces Int16

deriving via Shown Int16 instance ToQueryParamString Int16

deriving via Readed Int16 instance FromPathPieces Int16

deriving via Readed Int16 instance FromQueryParamString Int16

deriving via Shown Int32 instance ToPathPieces Int32

deriving via Shown Int32 instance ToQueryParamString Int32

deriving via Readed Int32 instance FromPathPieces Int32

deriving via Readed Int32 instance FromQueryParamString Int32

deriving via Shown Int64 instance ToPathPieces Int64

deriving via Shown Int64 instance ToQueryParamString Int64

deriving via Readed Int64 instance FromPathPieces Int64

deriving via Readed Int64 instance FromQueryParamString Int64

deriving via Shown Word instance ToPathPieces Word

deriving via Shown Word instance ToQueryParamString Word

deriving via Readed Word instance FromPathPieces Word

deriving via Readed Word instance FromQueryParamString Word

deriving via Shown Word8 instance ToPathPieces Word8

deriving via Shown Word8 instance ToQueryParamString Word8

deriving via Readed Word8 instance FromPathPieces Word8

deriving via Readed Word8 instance FromQueryParamString Word8

deriving via Shown Word16 instance ToPathPieces Word16

deriving via Shown Word16 instance ToQueryParamString Word16

deriving via Readed Word16 instance FromPathPieces Word16

deriving via Readed Word16 instance FromQueryParamString Word16

deriving via Shown Word32 instance ToPathPieces Word32

deriving via Shown Word32 instance ToQueryParamString Word32

deriving via Readed Word32 instance FromPathPieces Word32

deriving via Readed Word32 instance FromQueryParamString Word32

deriving via Shown Word64 instance ToPathPieces Word64

deriving via Shown Word64 instance ToQueryParamString Word64

deriving via Readed Word64 instance FromPathPieces Word64

deriving via Readed Word64 instance FromQueryParamString Word64

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
  , queryString :: !DQuery
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
  , body :: !LBS.ByteString
  , pathInfo :: ![T.Text]
  , queryString :: Query
  }
  deriving (Show, Generic)

data StewardResponse = StewardResponse
  { status :: !Status
  , headers :: !ResponseHeaders
  , body :: !LBS.ByteString
  }
  deriving (Generic)

class (AppHList (RouteArgs a)) => PreRoutable a where
  type ResponseSeed a :: Type
  type RouteArgs a :: [Type]
  buildRequest' :: Proxy# a -> HList (RouteArgs a) -> PartialRequest
  decodeResponseBody' :: Proxy# a -> LBS.ByteString -> Either String (ResponseSeed a)

buildRequest ::
  forall a ->
  (PreRoutable a) =>
  HList (RouteArgs a) ->
  PartialRequest
buildRequest a = buildRequest' (proxy# @a)

class (PreRoutable a) => Routable m a where
  matchRoute' ::
    Proxy# a ->
    StewardRequest ->
    [T.Text] ->
    ParseResult ((RouteArgs a ~> m (ResponseSeed a)) -> m StewardResponse)

matchRoute ::
  forall a ->
  (Routable m a) =>
  StewardRequest ->
  [T.Text] ->
  ParseResult ((RouteArgs a ~> m (ResponseSeed a)) -> m StewardResponse)
matchRoute a = matchRoute' (proxy# @a)

instance (KnownSymbol s, PreRoutable t) => PreRoutable (s /> t) where
  type RouteArgs (s /> t) = RouteArgs t
  type ResponseSeed (s /> t) = ResponseSeed t
  buildRequest' _ =
    (#pathInfo %~ (T.pack (symbolVal' @s proxy#) :))
      . buildRequest' (proxy# @t)
  decodeResponseBody' _ = decodeResponseBody' @t proxy#

instance (KnownSymbol s, Routable m t) => Routable m (s /> t) where
  matchRoute' _ req (x : xs)
    | x == T.pack (symbolVal' @s proxy#) =
        matchRoute' (proxy# @t) req xs
  matchRoute' _ _ _ = NoMatch

instance (KnownSymbols ss, PreRoutable t) => PreRoutable (ss /> t) where
  type RouteArgs (ss /> t) = RouteArgs t
  type ResponseSeed (ss /> t) = ResponseSeed t
  buildRequest' _ =
    (#pathInfo %~ (symbolsVal ss <>))
      . buildRequest' (proxy# @t)
  decodeResponseBody' _ = decodeResponseBody' @t proxy#

instance (KnownSymbols ss, Routable m t) => Routable m (ss /> t) where
  matchRoute' _ req xs
    | Just ys <- List.stripPrefix (symbolsVal ss) xs =
        matchRoute' (proxy# @t) req ys
  matchRoute' _ _ _ = NoMatch

instance
  (ToPathPieces x, PreRoutable t) =>
  PreRoutable (x /> t)
  where
  type RouteArgs (x /> t) = x : RouteArgs t
  type ResponseSeed (x /> t) = ResponseSeed t
  buildRequest' _ (x :- xs) =
    buildRequest' (proxy# @t) xs
      & #pathInfo %~ (toPathPieces x <>)
  decodeResponseBody' _ = decodeResponseBody' @t proxy#

instance
  (ToPathPieces x, FromPathPieces x, Routable m t) =>
  Routable m (x /> t)
  where
  matchRoute' _ req xs = case parsePathPieces @x xs of
    NoMatch -> NoMatch
    Failed e -> Failed e
    Parsed (a, ys) -> case matchRoute' (proxy# @t) req ys of
      NoMatch -> NoMatch
      Failed e -> Failed e
      Parsed f -> Parsed \g -> f (g a)

type data Modifier = Header Symbol | JSONBody Type | RawRequestBody | QueryParam Type

instance (KnownSymbol s, PreRoutable t) => PreRoutable (Header s /> t) where
  type RouteArgs (Header s /> t) = Maybe BS.ByteString ': RouteArgs t
  type ResponseSeed (Header s /> t) = ResponseSeed t
  buildRequest' _ (mhdr :- xs) =
    let headerName = CI.mk $ TE.encodeUtf8 $ T.pack $ symbolVal' @s proxy#
     in buildRequest' (proxy# @t) xs
          & #headers
            %~ maybe id ((:) . (headerName,)) mhdr . filter ((/= headerName) . fst)
  decodeResponseBody' _ = decodeResponseBody' @t proxy#

instance (KnownSymbol s, Routable m t) => Routable m (Header s /> t) where
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
  type ResponseSeed (RawRequestBody /> t) = ResponseSeed t
  buildRequest' _ (a :- xs) =
    buildRequest' (proxy# @t) xs & #body .~ a
  decodeResponseBody' _ = decodeResponseBody' @t proxy#

instance (Routable m t) => Routable m (RawRequestBody /> t) where
  matchRoute' _ req xs =
    case matchRoute' (proxy# @t) req xs of
      NoMatch -> NoMatch
      Failed e -> Failed e
      Parsed f -> Parsed \g -> f (g req.body)

class FromQueryParams a where
  parseQueryParams :: Query -> ParseResult a

type GenericFromQueryParams a = (Generic a, GFromQueryParams (Generics.Rep a))

instance (GenericFromQueryParams a) => FromQueryParams (Generically a) where
  parseQueryParams = fmap (Generically . Generics.to) . gparseQueryParams

instance FromQueryParams Query where
  parseQueryParams = Parsed

class GFromQueryParams f where
  gparseQueryParams :: Query -> ParseResult (f ())

instance GFromQueryParams U1 where
  gparseQueryParams _ = Parsed U1

instance
  {-# OVERLAPPING #-}
  (KnownSymbol s) =>
  GFromQueryParams (M1 S ('MetaSel ('Just s) x y z) (K1 i Bool))
  where
  gparseQueryParams qs =
    let fld = symbolVal' @s proxy#
     in case lookup (TE.encodeUtf8 $ T.pack fld) qs of
          Nothing -> Parsed $ M1 $ K1 False
          Just Nothing -> Parsed $ M1 $ K1 True
          Just (Just v) ->
            Failed $ "Query Flag expected, but got a value: " <> show (fld, v)

instance
  {-# OVERLAPPING #-}
  (KnownSymbol s, FromQueryParamString a) =>
  GFromQueryParams (M1 S ('MetaSel ('Just s) x y z) (K1 i (Maybe a)))
  where
  gparseQueryParams qs =
    let fld = symbolVal' @s proxy#
     in case lookup (TE.encodeUtf8 $ T.pack fld) qs of
          Nothing -> Parsed $ M1 $ K1 Nothing
          Just Nothing -> Failed $ "No associated value in query: " <> fld
          Just (Just x) -> case parseQueryParamString x of
            NoMatch -> Failed $ "Query field (" <> fld <> ") doesn't parse"
            Failed e -> Failed $ "Parse fail in query (" <> fld <> "): " <> e
            Parsed a -> Parsed $ M1 $ K1 $ Just a

instance
  {-# OVERLAPPABLE #-}
  (KnownSymbol s, FromQueryParamString a) =>
  GFromQueryParams (M1 S ('MetaSel ('Just s) x y z) (K1 i a))
  where
  gparseQueryParams qs =
    let fld = symbolVal' @s proxy#
     in case lookup (TE.encodeUtf8 $ T.pack fld) qs of
          Nothing -> Failed $ "No query parameter: " <> fld
          Just Nothing -> Failed $ "No associated value in query: " <> fld
          Just (Just x) -> case parseQueryParamString x of
            NoMatch -> NoMatch
            Failed e -> Failed e
            Parsed a -> Parsed $ M1 $ K1 a

instance (GFromQueryParams l, GFromQueryParams r) => GFromQueryParams (l :*: r) where
  gparseQueryParams qs = (:*:) <$> gparseQueryParams qs <*> gparseQueryParams qs

instance {-# OVERLAPPABLE #-} (GFromQueryParams f) => GFromQueryParams (M1 i c f) where
  gparseQueryParams qs = M1 <$> gparseQueryParams qs

type DQuery = DList QueryItem

class ToQueryParams a where
  toQueryParams :: a -> DQuery

type GenericToQueryParams a = (Generic a, GToQueryParams (Generics.Rep a))

instance (GenericToQueryParams a) => ToQueryParams (Generically a) where
  toQueryParams = gtoQueryParams . Generics.from . (\(Generically a) -> a)

instance ToQueryParams Query where
  toQueryParams = DL.fromList

class GToQueryParams f where
  gtoQueryParams :: f a -> DQuery

instance GToQueryParams U1 where
  gtoQueryParams _ = mempty

instance
  {-# OVERLAPPABLE #-}
  (KnownSymbol s, ToQueryParamString a) =>
  GToQueryParams (M1 S ('MetaSel ('Just s) x y z) (K1 i (Maybe a)))
  where
  gtoQueryParams (M1 (K1 (Just x))) =
    DL.singleton
      ( TE.encodeUtf8 $ T.pack $ symbolVal' @s proxy#
      , Just $ toQueryParamString x
      )
  gtoQueryParams (M1 (K1 Nothing)) = mempty

instance
  {-# OVERLAPPABLE #-}
  (KnownSymbol s, ToQueryParamString a) =>
  GToQueryParams (M1 S ('MetaSel ('Just s) x y z) (K1 i a))
  where
  gtoQueryParams (M1 (K1 x)) =
    DL.singleton
      ( TE.encodeUtf8 $ T.pack $ symbolVal' @s proxy#
      , Just $ toQueryParamString x
      )

instance
  {-# OVERLAPPING #-}
  (KnownSymbol s) =>
  GToQueryParams (M1 S ('MetaSel ('Just s) x y z) (K1 i Bool))
  where
  gtoQueryParams (M1 (K1 p))
    | p = DL.singleton (TE.encodeUtf8 $ T.pack $ symbolVal' @s proxy#, Nothing)
    | otherwise = mempty

instance (GToQueryParams l, GToQueryParams r) => GToQueryParams (l :*: r) where
  gtoQueryParams (l :*: r) = gtoQueryParams l <> gtoQueryParams r

instance {-# OVERLAPPABLE #-} (GToQueryParams f) => GToQueryParams (M1 i c f) where
  gtoQueryParams (M1 x) = gtoQueryParams x

instance (ToQueryParams a, PreRoutable t) => PreRoutable (QueryParam a /> t) where
  type RouteArgs (QueryParam a /> t) = a ': RouteArgs t
  type ResponseSeed (QueryParam a /> t) = ResponseSeed t
  buildRequest' _ (a :- xs) =
    buildRequest' (proxy# @t) xs
      & #queryString %~ (toQueryParams a <>)
  decodeResponseBody' _ = decodeResponseBody' @t proxy#

instance
  (ToQueryParams a, FromQueryParams a, Routable m t) =>
  Routable m (QueryParam a /> t)
  where
  matchRoute' _ req xs = case parseQueryParams (req.queryString) of
    NoMatch -> NoMatch
    Failed e -> Failed e
    Parsed a -> case matchRoute' (proxy# @t) req xs of
      NoMatch -> NoMatch
      Failed e -> Failed e
      Parsed f -> Parsed \g -> f (g a)

instance (ToJSON a, PreRoutable t) => PreRoutable (JSONBody a /> t) where
  type RouteArgs (JSONBody a /> t) = a ': RouteArgs t
  type ResponseSeed (JSONBody a /> t) = ResponseSeed t
  buildRequest' _ (a :- xs) =
    buildRequest' (proxy# @t) xs
      & #body .~ J.encode a
  decodeResponseBody' _ = decodeResponseBody' @t proxy#

instance (ToJSON a, FromJSON a, Routable m t) => Routable m (JSONBody a /> t) where
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
  (KnownMethod meth, IsResponseType responseType) =>
  PreRoutable (Verb meth statCode responseType)
  where
  type RouteArgs (Verb meth statCode responseType) = '[]
  type ResponseSeed (Verb meth statCode responseType) = ResponseContent responseType
  buildRequest' _ _ =
    PartialRequest
      { pathInfo = []
      , queryString = mempty
      , method = methodVal meth
      , body = mempty
      , headers = []
      }
  decodeResponseBody' _ = decodeResponseType' (proxy# @responseType)

instance
  (Monad m, KnownMethod meth, KnownNat statCode, IsResponseType responseType) =>
  Routable m (Verb meth statCode responseType)
  where
  matchRoute' _ req []
    | req.method == methodVal meth =
        Parsed \get -> do
          body <- encodeResponse responseType <$> get
          pure $
            StewardResponse
              { status =
                  Status
                    { statusMessage = mempty
                    , statusCode = fromIntegral $ natVal' @statCode proxy#
                    }
              , headers = [(hContentType, Media.renderHeader $ mediaType @responseType proxy#)]
              , body = body
              }
    | otherwise = NoMatch
  matchRoute' _ _ _ = NoMatch

data ResponseType = JSON Type | PlainText | NoContent | HTML
  deriving (Generic)

type IsResponseType :: ResponseType -> Constraint
class IsResponseType rt where
  type ResponseContent rt :: a
  mediaType :: Proxy# rt -> MediaType
  encodeResponse' :: Proxy# rt -> ResponseContent rt -> LBS.ByteString
  decodeResponseType' :: Proxy# rt -> LBS.ByteString -> Either String (ResponseContent rt)

instance (J.FromJSON a, J.ToJSON a) => IsResponseType (JSON a) where
  type ResponseContent (JSON a) = a
  mediaType _ = "application" // "json"
  encodeResponse' _ = J.encode
  decodeResponseType' _ = J.eitherDecode

instance IsResponseType PlainText where
  type ResponseContent PlainText = LT.Text
  mediaType _ = "text" // "plain"
  encodeResponse' _ = LTE.encodeUtf8
  decodeResponseType' _ = Right . LTE.decodeUtf8

instance IsResponseType NoContent where
  type ResponseContent NoContent = ()
  mediaType _ = "text" // "plain"
  encodeResponse' _ _ = LBS.empty
  decodeResponseType' _ _ = Right ()

instance IsResponseType HTML where
  type ResponseContent HTML = Either LBS.ByteString (Html ())
  mediaType _ = "text" // "html"
  encodeResponse' _ = either id Lucid.renderBS
  decodeResponseType' _ = Right . Left

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

newtype instance Handler m ::: api = Handler (RouteArgs api ~> m (ResponseSeed api))

type Client :: (Type -> Type) -> Type
data Client m

newtype instance Client m ::: api = Client {call :: RouteArgs api ~> m (ResponseSeed api)}

class (MonadThrow m) => MonadClient m where
  request :: PartialRequest -> m StewardResponse

type Application m = StewardRequest -> m StewardResponse

class (Monad m) => MonadHandler m where
  runApp :: Application m -> m ()

toApplication ::
  (Applicative m, HasHandler m t) =>
  t (Handler m) ->
  StewardRequest ->
  m StewardResponse
toApplication hs req = case parseApplication hs req req.pathInfo of
  NoMatch ->
    pure
      StewardResponse
        { status = status404
        , headers = mempty
        , body = "Not Found: " <> LTE.encodeUtf8 (LT.fromStrict $ T.intercalate "/" req.pathInfo)
        }
  Failed e ->
    pure
      StewardResponse
        { status = status500
        , headers = mempty
        , body = fromString e
        }
  Parsed f -> f

class HasHandler m t where
  parseApplication ::
    t (Handler m) ->
    StewardRequest ->
    [T.Text] ->
    ParseResult (m StewardResponse)
  default parseApplication ::
    (GenericHasHandler m (t (Handler m))) =>
    t (Handler m) ->
    StewardRequest ->
    [T.Text] ->
    ParseResult (m StewardResponse)
  parseApplication = gparseApplication . Generics.from

type GenericHasHandler m a = (Generic a, GHandler m (Rep a))

class GHandler m f where
  gparseApplication :: f (Handler m) -> StewardRequest -> [T.Text] -> ParseResult (m StewardResponse)

instance (GHandler m f) => GHandler m (M1 i c f) where
  gparseApplication (M1 x) = gparseApplication x

instance {-# OVERLAPPING #-} (Routable m t) => GHandler m (K1 i (Handler m ::: t)) where
  gparseApplication (K1 (Handler f)) r pinfo = case matchRoute t r pinfo of
    NoMatch -> NoMatch
    Failed e -> Failed e
    Parsed g -> Parsed $ g f

instance
  {-# OVERLAPPABLE #-}
  (HasHandler m t) =>
  GHandler m (K1 i (t (Handler m)))
  where
  gparseApplication (K1 x) = parseApplication x

instance (GHandler m l, GHandler m r) => GHandler m (l :*: r) where
  gparseApplication (l :*: r) req pinfo = case gparseApplication l req pinfo of
    NoMatch -> gparseApplication r req pinfo
    Failed e -> Failed e
    Parsed f -> Parsed f

runHandlers :: (MonadHandler m, HasHandler m t) => t (Handler m) -> m ()
runHandlers = runApp . toApplication

data ClientException = InvalidResponseBody String
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

class HasClient t where
  client :: (MonadClient m) => t (Client m)
  default client ::
    forall m.
    (MonadClient m, GenericHasClient (t (Client m))) =>
    t (Client m)
  client = Generics.to $ gclient @(Rep (t (Client m))) @m

type GenericHasClient a = (Generic a, GHasClient (Rep a))

class GHasClient f where
  gclient :: (MonadClient m) => f (Client m)

instance (GHasClient f) => GHasClient (M1 i c f) where
  gclient = M1 gclient
  {-# INLINE gclient #-}

instance (GHasClient l, GHasClient r) => GHasClient (l :*: r) where
  gclient = gclient :*: gclient
  {-# INLINE gclient #-}

instance (PreRoutable t, MonadClient m) => GHasClient (K1 i (Client m ::: t)) where
  gclient =
    K1 $
      Client $
        appHList @(RouteArgs t)
          ( either (throwM . InvalidResponseBody) pure
              . decodeResponseBody' @t proxy#
              . (.body)
              <=< request @m . buildRequest t
          )
  {-# INLINE gclient #-}
