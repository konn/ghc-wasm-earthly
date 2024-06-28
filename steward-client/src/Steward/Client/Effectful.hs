{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Steward.Client.Effectful (StewardClient, URI, parseURI, runStewardClient, makeRawRequest) where

import Effectful
import Effectful.Dispatch.Static
import Network.HTTP.Client.TLS (newTlsManager)
import Network.URI
import Steward.Client
import Steward.Types

data StewardClient :: Effect

type instance DispatchOf StewardClient = Static 'WithSideEffects

newtype instance StaticRep StewardClient = StewardClientEnv ClientEnv

runStewardClient :: (IOE :> es) => URI -> Eff (StewardClient ': es) a -> Eff es a
runStewardClient uri act = do
  man <- newTlsManager
  evalStaticRep (StewardClientEnv ClientEnv {manager = man, endpoint = uri}) act

instance (StewardClient :> es) => MonadClient (Eff es) where
  request = makeRawRequest

makeRawRequest :: (StewardClient :> es) => PartialRequest -> Eff es StewardResponse
makeRawRequest req = do
  StewardClientEnv env <- getStaticRep
  unsafeEff_ $ makeStewardRequest env req
