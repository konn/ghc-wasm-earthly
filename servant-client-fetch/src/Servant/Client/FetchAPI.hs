{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Servant.Client.FetchAPI (
  FetchT (),
  FetchM,
  runFetch,
  runFetchWith,
  module Servant.Client.Core.Reexport,
) where

import Control.Exception (throwIO)
import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow, throwString)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Reader
import Data.Bitraversable qualified as Bi
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Foldable qualified as F
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Sequence qualified as Seq
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Prim
import GHC.Wasm.Web.Generated.BodyInit.Core (BodyInit)
import GHC.Wasm.Web.Generated.Headers qualified as Hdrs
import GHC.Wasm.Web.Generated.RequestInfo
import GHC.Wasm.Web.Generated.RequestInit
import GHC.Wasm.Web.Generated.Response
import GHC.Wasm.Web.Generated.Response qualified as FetchResp
import GHC.Wasm.Web.Generated.URL
import GHC.Wasm.Web.ReadableStream (fromReadableStream, toReadableStream)
import Network.HTTP.Media (renderHeader)
import Network.HTTP.Types.Status (Status (..))
import Network.HTTP.Types.URI (renderQuery)
import Network.HTTP.Types.Version (HttpVersion)
import Servant.API (SourceIO)
import Servant.Client.Core
import Servant.Client.Core qualified as Servant
import Servant.Client.Core.Reexport
import Servant.Types.SourceT qualified as Servant
import Streaming.ByteString qualified as Q
import Streaming.ByteString.Internal qualified as QI
import Streaming.Prelude qualified as S
import Wasm.Prelude.Linear qualified as PL

newtype FetchT m a = FetchT (ReaderT FetchEnv m a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving newtype (MonadThrow, MonadCatch, MonadUnliftIO, MonadMask)
  deriving (Semigroup, Monoid) via Ap (FetchT m) a

type FetchM = FetchT IO

data FetchEnv = FetchEnv {fetcher :: !Fetcher, baseUrl :: !BaseUrl}
  deriving (Generic)

runFetch :: BaseUrl -> FetchT m a -> m a
runFetch burl (FetchT act) =
  runReaderT
    act
    FetchEnv
      { fetcher = js_toplevel_fetch
      , baseUrl = burl
      }

runFetchWith :: JSObject cls -> BaseUrl -> FetchT m a -> m a
runFetchWith obj burl (FetchT act) =
  runReaderT
    act
    FetchEnv
      { fetcher = js_fetch_of obj
      , baseUrl = burl
      }

instance (MonadIO m) => RunClient (FetchT m) where
  throwClientError = liftIO . throwIO
  runRequestAcceptStatus _ req = do
    FetchEnv {..} <- FetchT ask
    liftIO do
      let path =
            fromText $
              TE.decodeUtf8 $
                LBS.toStrict $
                  BB.toLazyByteString req.requestPath
          base = toUSVString $ toJSString $ showBaseUrl baseUrl
      url <- liftIO $ js_cons_URL path $ nonNull base
      liftIO $ do
        unless (null req.requestQueryString) $
          js_set_search url $
            fromText $
              TE.decodeUtf8 $
                renderQuery True $
                  F.toList req.requestQueryString
      meth <- fromHaskellByteString req.requestMethod
      let headerSeeds =
            addCType $
              addAccept $
                filter ((`notElem` ["Content-Type", "Accept"]) . fst) $
                  F.toList req.requestHeaders
          addAccept
            | null req.requestAccept = id
            | otherwise = (("Accept", renderHeader $ F.toList req.requestAccept) :)
          addCType = maybe id ((:) . ("Content-Type",) . renderHeader . snd) req.requestBody
      hdrs <-
        toJSRecord @JSByteStringClass @JSByteStringClass
          . Map.fromList
          =<< mapM
            (Bi.bitraverse (pure . TE.decodeUtf8 . CI.original) fromHaskellByteString)
            headerSeeds
      mbody <- mapM (fromBody . fst) req.requestBody
      let reqInit =
            newDictionary @RequestInitFields do
              maybe PL.id (setPartialField "body" . nonNull . nonNull) mbody
                PL.. setPartialField "method" (nonNull meth)
                PL.. setPartialField "headers" (nonNull $ inject hdrs)
      fromResp req.requestHttpVersion
        =<< await
        =<< fetcher (unsafeCast url) (nonNull reqInit)

fromBody :: Servant.RequestBody -> IO BodyInit
fromBody (Servant.RequestBodyLBS lbs) =
  fmap inject $ toReadableStream $ Q.fromLazy lbs
fromBody (Servant.RequestBodyBS bs) =
  fmap inject $ toReadableStream $ Q.fromStrict bs
fromBody (Servant.RequestBodySource src) =
  fmap inject $ toReadableStream (fromSourceIOLBS src)

fromSourceIOLBS :: SourceIO LBS.ByteString -> Q.ByteStream IO ()
fromSourceIOLBS (Servant.SourceT withSteps) = QI.Go $ withSteps $ pure . go
  where
    go :: Servant.StepT IO LBS.ByteString -> Q.ByteStream IO ()
    go = \case
      Servant.Stop -> mempty
      Servant.Yield lbs stp -> Q.fromLazy lbs >> go stp
      Servant.Error err -> liftIO $ throwString err
      Servant.Effect es -> QI.Go $ go <$> es
      Servant.Skip a -> go a

fromResp :: HttpVersion -> FetchResp.Response -> IO Servant.Response
fromResp ver resp = do
  statusCode <- fromIntegral <$> FetchResp.js_get_status resp
  statusMessage <- toHaskellByteString =<< FetchResp.js_get_statusText resp
  responseHeaders <-
    fmap Seq.fromList
      . S.toList_
      . S.mapM (Bi.bitraverse (fmap CI.mk . toHaskellByteString) toHaskellByteString)
      . fromPairIterable
      =<< Hdrs.js_iter_Headers_ByteString_ByteString
      =<< FetchResp.js_get_headers resp
  mbody <- fromNullable <$> FetchResp.js_get_body resp
  let responseStatusCode = Status {..}
      responseHttpVersion = ver
  responseBody <- case mbody of
    Nothing -> pure mempty
    Just rst -> Q.toLazy_ $ fromReadableStream rst
  pure Servant.Response {..}

type Fetcher =
  RequestInfo ->
  Nullable RequestInitClass ->
  IO (Promise ResponseClass)

foreign import javascript safe "fetch($1, $2)"
  js_toplevel_fetch :: Fetcher

foreign import javascript safe "$1.fetch($2, $3)"
  js_fetch_of :: JSObject a -> Fetcher
