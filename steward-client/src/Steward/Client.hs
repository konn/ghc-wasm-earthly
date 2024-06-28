{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Steward.Client (
  runClient,
  runClientWith,
  ClientT (..),
  ClientEnv (..),

  -- * Internal functions
  toStewardResponse,
  fromStewardRequest,
  makeStewardRequest,
) where

import Control.Exception.Safe (Exception, MonadCatch, MonadThrow, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Functor ((<&>))
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
  runReaderT act ClientEnv {manager = man, endpoint = uri}

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
    , MonadReader ClientEnv
    )

data ClientEnv = ClientEnv
  { manager :: !Manager
  , endpoint :: !URI
  }
  deriving (Generic)

instance (MonadThrow m, MonadIO m) => MonadClient (ClientT m) where
  request req = do
    env <- ask
    ClientT $ liftIO $ makeStewardRequest env req

makeStewardRequest :: ClientEnv -> PartialRequest -> IO StewardResponse
makeStewardRequest env req = do
  req' <- fromStewardRequest env.endpoint req
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
      , H.queryString = renderQuery True req.queryString
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
