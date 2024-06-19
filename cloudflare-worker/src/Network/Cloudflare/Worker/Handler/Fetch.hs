{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Network.Cloudflare.Worker.Handler.Fetch (
  FetchHandler,
  FetchHandlerClass,
  toFetchHandler,
  FetchContext,
  FetchContextClass,
  waitUntil,
  passThroughOnException,
) where

import Data.Text qualified as T
import Data.Word
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Prim
import Network.Cloudflare.Worker.Request (WorkerRequest)
import Network.Cloudflare.Worker.Response (WorkerResponse)

type data FetchHandlerClass :: Prototype

type instance SuperclassOf FetchHandlerClass = 'Nothing

type FetchHandler = JSObject FetchHandlerClass

type data FetchContextClass :: Prototype

type instance SuperclassOf FetchContextClass = 'Nothing

type FetchContext = JSObject FetchContextClass

foreign import javascript unsafe "$1.waitUntil($2)"
  waitUntil :: FetchContext -> Promise c -> IO ()

foreign import javascript unsafe "$1.passThroughOnException()"
  passThroughOnException :: FetchContext -> IO ()

foreign import javascript unsafe "wrapper"
  toFetchHandler ::
    (WorkerRequest -> JSAny -> FetchContext -> IO WorkerResponse) -> FetchHandler
