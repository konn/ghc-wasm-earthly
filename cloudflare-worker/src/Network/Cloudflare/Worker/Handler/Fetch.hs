{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Network.Cloudflare.Worker.Handler.Fetch (
  FetchHandler,
  JSFetchHandler,
  JSFetchHandlerClass,
  toJSFetchHandler,
  FetchContext,
  FetchContextClass,
  waitUntil,
  passThroughOnException,
) where

import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Request (WorkerRequest)
import Network.Cloudflare.Worker.Response (WorkerResponse)

type data JSFetchHandlerClass :: Prototype

type instance SuperclassOf JSFetchHandlerClass = 'Nothing

type JSFetchHandler = JSObject JSFetchHandlerClass

type data FetchContextClass :: Prototype

type instance SuperclassOf FetchContextClass = 'Nothing

type FetchContext = JSObject FetchContextClass

foreign import javascript unsafe "$1.waitUntil($2)"
  waitUntil :: FetchContext -> Promise c -> IO ()

foreign import javascript unsafe "$1.passThroughOnException()"
  passThroughOnException :: FetchContext -> IO ()

type FetchHandler env = WorkerRequest -> JSObject env -> FetchContext -> IO WorkerResponse

foreign import javascript unsafe "wrapper"
  toJSFetchHandler :: FetchHandler env -> IO JSFetchHandler
