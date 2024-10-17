{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Steward.Client.Fetch (
  ClientM,
  runClientOf,
  runClient,
  runClientWith,
) where

import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import qualified Data.Bitraversable as Bi
import qualified Data.ByteString as LBS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import GHC.Wasm.Object.Builtins
import qualified GHC.Wasm.Web.Generated.Headers as Headers
import GHC.Wasm.Web.Generated.RequestInfo (RequestInfo)
import GHC.Wasm.Web.Generated.RequestInit (RequestInitClass)
import GHC.Wasm.Web.Generated.RequestInit.Core (RequestInitFields)
import GHC.Wasm.Web.Generated.Response (ResponseClass)
import qualified GHC.Wasm.Web.Generated.Response as Resp
import GHC.Wasm.Web.Generated.URL (js_cons_URL, js_set_search)
import GHC.Wasm.Web.ReadableStream (fromReadableStream)
import qualified Network.HTTP.Types as H
import Network.HTTP.Types.URI (encodePathSegments, renderQuery)
import Steward.Types
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as S
import qualified Wasm.Prelude.Linear as PL

data ClientEnv = ClientEnv {base :: String, fetcher :: Fetcher}

newtype ClientM a = ClientM (ReaderT ClientEnv IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadMask
    , MonadCatch
    )

runClient :: (MonadIO m) => String -> ClientM a -> m a
runClient base (ClientM act) =
  let fetcher = js_toplevel_fetch
   in liftIO $ runReaderT act ClientEnv {..}

runClientOf :: (MonadIO m) => JSObject obj -> String -> ClientM a -> m a
runClientOf obj base (ClientM act) =
  let fetcher = js_fetch_of obj
   in liftIO $ runReaderT act ClientEnv {..}

runClientWith :: (MonadIO m) => Fetcher -> String -> ClientM a -> m a
runClientWith fetcher base (ClientM act) =
  liftIO $ runReaderT act ClientEnv {..}

instance MonadClient ClientM where
  request preq = do
    ClientEnv {..} <- ClientM ask
    let path =
          fromText $
            TE.decodeUtf8 $
              LBS.toStrict $
                BB.toLazyByteString $
                  encodePathSegments preq.pathInfo
    url <- liftIO $ js_cons_URL path $ nonNull $ fromText $ T.pack base
    liftIO $ do
      unless (null preq.queryString) $
        js_set_search url $
          fromText $
            TE.decodeUtf8 $
              renderQuery True $
                F.toList preq.queryString
    resp <- liftIO $ useByteStringAsJSByteArray @Word8 (LBS.toStrict preq.body) \mbody -> do
      reqHeaders <-
        toJSRecord @JSByteStringClass @JSByteStringClass
          . Map.fromList
          =<< mapM (Bi.bitraverse (pure . TE.decodeUtf8 . CI.original) fromHaskellByteString) preq.headers
      meth <- liftIO $ fromHaskellByteString $ BS8.pack $ show preq.method
      let reqInit =
            newDictionary @RequestInitFields do
              setPartialField "body" (nonNull (nonNull $ inject mbody))
                PL.. setPartialField "method" (nonNull meth)
                PL.. setPartialField "headers" (nonNull $ inject reqHeaders)
      consoleLog =<< stringify reqInit
      await =<< fetcher (unsafeCast url) (nonNull reqInit)
    statusCode <- liftIO $ Resp.js_get_status resp
    statusText <-
      liftIO $
        toHaskellByteString
          =<< Resp.js_get_statusText resp
    headers <-
      liftIO $
        S.toList_
          . S.mapM (Bi.bitraverse (fmap CI.mk . toHaskellByteString) toHaskellByteString)
          . fromPairIterable
          =<< Headers.js_iter_Headers_ByteString_ByteString
          =<< Resp.js_get_headers resp
    body <-
      liftIO $
        maybe (pure "") (Q.toLazy_ . fromReadableStream)
          . fromNullable
          =<< Resp.js_get_body resp
    pure
      StewardResponse
        { status =
            H.Status
              { statusCode = fromIntegral statusCode
              , statusMessage = statusText
              }
        , headers = headers
        , body = body
        }

type Fetcher =
  RequestInfo ->
  Nullable RequestInitClass ->
  IO (Promise ResponseClass)

foreign import javascript safe "fetch($1, $2)"
  js_toplevel_fetch :: Fetcher

foreign import javascript safe "$1.fetch($2, $3)"
  js_fetch_of :: JSObject a -> Fetcher

foreign import javascript unsafe "console.log($1)"
  consoleLog :: USVString -> IO ()

foreign import javascript unsafe "JSON.stringify($1)"
  stringify :: JSObject a -> IO USVString
