{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Steward.Workers (
  Worker,
  runWorker,
  getContext,
  getWorkerEnv,
  fromHandlers,

  -- * Re-exports
  JSObject (..),
  StewardRequest (..),
  StewardResponse (..),
  PartialRequest (..),
  module Steward.Types,
) where

import Control.Exception.Safe (Exception, SomeException, displayException, handleAny, throwM)
import Data.Bifunctor qualified as Bi
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive qualified as CI
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import Effectful
import Effectful.Dispatch.Static
import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Prim (fromJSString)
import Network.Cloudflare.Worker.Handler.Fetch
import Network.Cloudflare.Worker.Request (WorkerRequest)
import Network.Cloudflare.Worker.Request qualified as Req
import Network.Cloudflare.Worker.Response (SimpleResponseInit (..), WorkerResponse)
import Network.Cloudflare.Worker.Response qualified as Resp
import Network.HTTP.Types.Method (parseMethod)
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI (decodePathSegments, parseQuery)
import Network.URI
import Steward.Types
import Streaming (hoist)
import Streaming.ByteString qualified as Q

data Worker :: Prototype -> Effect

type instance DispatchOf (Worker e) = 'Static 'WithSideEffects

data instance StaticRep (Worker e) = WorkerEnv
  { env :: JSObject e
  , context :: FetchContext
  , request :: StewardRequest
  }
  deriving (Generic)

getContext :: forall e es. (Worker e :> es) => Eff es FetchContext
getContext = getStaticRep @(Worker e) >>= pure . context

getWorkerEnv :: forall e es. (Worker e :> es) => Eff es (JSObject e)
getWorkerEnv = getStaticRep @(Worker e) >>= pure . env

fromHandlers ::
  forall e es t.
  (Worker e :> es, HasHandler (Eff es) t) =>
  t (Handler (Eff es)) ->
  Eff es StewardResponse
fromHandlers t = do
  WorkerEnv {request = r} <- getStaticRep @(Worker e)
  toApplication t r

runWorker :: Eff '[Worker e, IOE] StewardResponse -> FetchHandler e
runWorker act req env context = runEff $ handleAny toErrorResp do
  req' <- toStewardRequest req
  fromStewardResponse
    =<< evalStaticRep WorkerEnv {request = req', ..} act

toErrorResp :: SomeException -> Eff '[IOE] WorkerResponse
toErrorResp exc =
  liftIO $
    Resp.newResponse
      SimpleResponseInit
        { statusText = "Internal Server Error"
        , status = 500
        , headers = mempty
        , body = fromString $ displayException exc
        }

fromStewardResponse :: (IOE :> es) => StewardResponse -> Eff es WorkerResponse
fromStewardResponse resp = liftIO do
  Resp.newResponse
    SimpleResponseInit
      { statusText = resp.status.statusMessage
      , status = fromIntegral resp.status.statusCode
      , headers = Map.fromList $ map (Bi.bimap (TE.decodeUtf8 . CI.original) TE.decodeUtf8) $ resp.headers
      , body = LT.toStrict $ LTE.decodeUtf8 resp.body
      }

data WorkersException = InvalidMethod BS8.ByteString
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

toStewardRequest :: (IOE :> es) => WorkerRequest -> Eff es StewardRequest
toStewardRequest req = do
  let uri = fromJust $ parseURI $ fromJSString $ Req.getUrl req
      secure = uri.uriScheme == "https:"
      host = BS8.pack (fromJust uri.uriAuthority).uriRegName
      port = maybe "" (BS8.pack . (.uriPort)) uri.uriAuthority
      rawQueryString = BS8.pack uri.uriQuery
      rawPathInfo = BS8.pack uri.uriPath
      queryString = parseQuery rawQueryString
      headers = map (Bi.first CI.mk) $ Req.getHeaders req
      pathInfo = decodePathSegments rawPathInfo
  body <- maybe mempty (Q.toLazy_ . hoist liftIO) $ Req.readBody req
  method <-
    either (throwM . InvalidMethod) pure . parseMethod
      =<< unsafeEff_ (toHaskellByteString (Req.getMethod req))
  pure StewardRequest {..}
