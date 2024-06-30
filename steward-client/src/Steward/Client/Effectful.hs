{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Steward.Client.Effectful (
  StewardClient,
  URI,
  parseURI,
  runStewardClient,
  makeRawRequest,
  withRequestModifier,
  withCloudflareServiceTokenAuth,
  ServiceToken (..),
) where

import Control.Monad ((<=<))
import Effectful
import Effectful.Dispatch.Static
import Network.HTTP.Client (Request)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.URI
import Steward.Client hiding (withCloudflareServiceTokenAuth, withRequestModifier)
import Steward.Types

data StewardClient :: Effect

type instance DispatchOf StewardClient = Static 'WithSideEffects

newtype instance StaticRep StewardClient = StewardClientEnv ClientEnv

runStewardClient :: (IOE :> es) => URI -> Eff (StewardClient ': es) a -> Eff es a
runStewardClient uri act = do
  man <- newTlsManager
  evalStaticRep (StewardClientEnv ClientEnv {manager = man, endpoint = uri, requestModifier = pure}) act

withRequestModifier ::
  (StewardClient :> es) =>
  (Request -> IO Request) ->
  Eff es a ->
  Eff es a
withRequestModifier f act = do
  localStaticRep (\(StewardClientEnv env) -> StewardClientEnv $ env {requestModifier = f <=< env.requestModifier}) act

withCloudflareServiceTokenAuth :: (StewardClient :> es) => ServiceToken -> Eff es a -> Eff es a
withCloudflareServiceTokenAuth tok =
  withRequestModifier \req -> pure $ addCloudflareAccessHeaders tok req

instance (StewardClient :> es) => MonadClient (Eff es) where
  request = makeRawRequest

makeRawRequest :: (StewardClient :> es) => PartialRequest -> Eff es StewardResponse
makeRawRequest req = do
  StewardClientEnv env <- getStaticRep
  unsafeEff_ $ makeStewardRequest env req
