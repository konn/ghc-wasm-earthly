{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Steward.Client (
  runClient,
  runClientWith,
  ClientT (..),
  ClientEnv (..),
  withRequestModifier,
  withCloudflareServiceTokenAuth,
  ServiceToken (..),

  -- * Internal functions
  addCloudflareAccessHeaders,
  toStewardResponse,
  fromStewardRequest,
  makeStewardRequest,
) where

import Control.Exception.Safe (Exception, MonadCatch, MonadMask, MonadThrow, throwIO)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader (local), ask)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable qualified as F
import Data.Functor ((<&>))
import Data.List qualified as List
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, Request, Response (..), httpLbs, requestFromURI)
import Network.HTTP.Client qualified as H
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (encodePathSegments, renderQuery, renderStdMethod, statusIsSuccessful)
import Network.HTTP.Types.Status (Status)
import Network.URI (URI (..))
import Steward.Types

runClient :: (MonadIO m) => URI -> ClientT m a -> m a
runClient uri act = do
  man <- newTlsManager
  runClientWith man uri act

runClientWith :: Manager -> URI -> ClientT m a -> m a
runClientWith man uri (ClientT act) = do
  runReaderT act ClientEnv {manager = man, endpoint = uri, requestModifier = pure}

data ServiceToken = ServiceToken
  { clientID :: BS8.ByteString
  , clientSecret :: BS8.ByteString
  }
  deriving (Show, Eq, Ord, Generic)

withCloudflareServiceTokenAuth ::
  (Monad m) =>
  ServiceToken ->
  ClientT m a ->
  ClientT m a
withCloudflareServiceTokenAuth tok = do
  withRequestModifier \req -> pure $ addCloudflareAccessHeaders tok req

addCloudflareAccessHeaders :: ServiceToken -> Request -> Request
addCloudflareAccessHeaders tok req =
  req
    { H.requestHeaders =
        (cfClientIdHeader, tok.clientID)
          : (cfClientSecretHeader, tok.clientSecret)
          : List.filter
            ((`notElem` [cfClientSecretHeader, cfClientIdHeader]) . fst)
            (H.requestHeaders req)
    }
  where
    cfClientIdHeader = "CF-Access-Client-Id"
    cfClientSecretHeader = "CF-Access-Client-Secret"

withRequestModifier ::
  (Monad m) =>
  (Request -> IO Request) ->
  ClientT m a ->
  ClientT m a
withRequestModifier f act = do
  local (\env -> env {requestModifier = f <=< env.requestModifier}) act

newtype ClientT m a = ClientT {unClientT :: ReaderT ClientEnv m a}
  deriving (Generic)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadTrans
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadReader ClientEnv
    )

data ClientEnv = ClientEnv
  { manager :: !Manager
  , endpoint :: !URI
  , requestModifier :: Request -> IO Request
  }
  deriving (Generic)

instance (MonadThrow m, MonadIO m) => MonadClient (ClientT m) where
  request req = do
    env <- ask
    ClientT $ liftIO $ makeStewardRequest env req

makeStewardRequest :: ClientEnv -> PartialRequest -> IO StewardResponse
makeStewardRequest env req = do
  req' <- env.requestModifier =<< fromStewardRequest env.endpoint req
  toStewardResponse =<< httpLbs req' env.manager

fromStewardRequest :: (MonadThrow m) => URI -> PartialRequest -> m Request
fromStewardRequest uri req =
  requestFromURI uri <&> \r ->
    r
      { H.method = renderStdMethod req.method
      , H.requestHeaders = req.headers
      , H.path =
          LBS.toStrict $
            BB.toLazyByteString $
              BB.byteString (BS8.dropWhileEnd (== '/') r.path)
                <> encodePathSegments req.pathInfo
      , H.queryString = renderQuery True (F.toList req.queryString)
      , H.requestBody = H.RequestBodyLBS req.body
      }

data StewardClientError = StatusCodeException Status LBS.ByteString
  deriving (Show, Generic)
  deriving anyclass (Exception)

toStewardResponse :: (MonadThrow m) => Response LBS.ByteString -> m StewardResponse
toStewardResponse rsp =
  if statusIsSuccessful rsp.responseStatus
    then
      pure
        StewardResponse
          { status = rsp.responseStatus
          , headers = rsp.responseHeaders
          , body = rsp.responseBody
          }
    else throwIO $ StatusCodeException rsp.responseStatus rsp.responseBody
