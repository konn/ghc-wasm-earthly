{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Servant.Workers (
  runWorker,
  Workers,
  WorkersEnv (..),

  -- * Environments
  getEnv,
  getSecret,
  getBinding,

  -- * Wrappers
  waitUntil,

  -- * Middlewars

  -- ** Cache
) where

import Control.Exception.Safe (try)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson (Value)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Effectful (MonadUnliftIO)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol)
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Binding (BindingsClass, ListMember)
import qualified Network.Cloudflare.Worker.Binding as Bind
import Network.Cloudflare.Worker.Handler.Fetch hiding (waitUntil)
import qualified Network.Cloudflare.Worker.Handler.Fetch as Fetch
import Network.Wai.Handler.Cloudflare.Workers
import Servant.API
import Servant.Server

newtype Workers e a = Workers {unWorkers :: ReaderT (WorkersEnv e) IO a}
  deriving (Generic)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader (WorkersEnv e)
    , MonadIO
    , MonadUnliftIO
    )

data WorkersEnv e = WorkersEnv
  { workerEnv :: !(JSObject e)
  , context :: !FetchContext
  }

serveR2 ::
  forall r2 ->
  (Member r2 bindings) =>
  String ->
  ServerT RawM (Workers (BindingsClass vars ss bindings))
serveR2 s prefix waiReq withResp = _

waitUntil :: Promise c -> Workers e ()
waitUntil p = do
  WorkersEnv {..} <- ask
  liftIO $ Fetch.waitUntil context p

getEnv ::
  forall s ->
  (KnownSymbol s, ListMember s vars) =>
  Workers (BindingsClass vars ss bs) Value
getEnv s = do
  WorkersEnv {..} <- ask
  pure $ Bind.getEnv s workerEnv

getSecret ::
  forall s ->
  (KnownSymbol s, ListMember s ss) =>
  Workers (BindingsClass vs ss bs) T.Text
getSecret s = do
  WorkersEnv {..} <- ask
  pure $ Bind.getSecret s workerEnv

getBinding ::
  forall s ->
  (Member s bs) =>
  Workers (BindingsClass vs ss bs) (JSObject (Lookup' s bs))
getBinding s = do
  WorkersEnv {..} <- ask
  pure $ Bind.getBinding s workerEnv

runWorker ::
  forall api e.
  (HasServer api '[]) =>
  ServerT api (Workers e) ->
  FetchHandler e
runWorker srv req env ctx =
  run
    ( serveWithContextT @api @'[] @(Workers e)
        (Proxy @api)
        EmptyContext
        ( \(Workers act) ->
            Handler $
              ExceptT $
                try $
                  runReaderT act WorkersEnv {workerEnv = env, context = ctx}
        )
        srv
    )
    req
    env
    ctx

data CacheConfig = CacheConfig
  { ignoreMethods :: !Bool
  , cacheTtl :: !Int
  , cacheErrors :: !Bool
  }
  deriving (Show, Eq, Ord, Generic)
