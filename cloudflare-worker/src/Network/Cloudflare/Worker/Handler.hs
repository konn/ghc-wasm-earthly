{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Network.Cloudflare.Worker.Handler (
  Handlers (..),
  toJSHandlers,
  JSHandlersFields,
  JSHandlersClass,
  JSHandlers,

  -- * Used as a Service Bindings
  fetchFrom,

  -- * Re-exports (for ffi exports)
  JSObject (..),
) where

import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Handler.Fetch
import Network.Cloudflare.Worker.Request (WorkerRequest)
import Network.Cloudflare.Worker.Response (WorkerResponseClass)

newtype Handlers env = Handlers {fetch :: FetchHandler env}
  deriving (Generic)

type JSHandlersFields = '[ '("fetch", JSFetchHandlerClass)]

type JSHandlersClass = JSDictionaryClass JSHandlersFields

type JSHandlers = JSObject JSHandlersClass

toJSHandlers :: Handlers envs -> IO JSHandlers
toJSHandlers Handlers {..} = do
  fetch' <- toJSFetchHandler fetch
  pure $ newDictionary @JSHandlersFields (setPartialField "fetch" fetch')

fetchFrom :: (handlers <: JSHandlersClass) => JSObject handlers -> WorkerRequest -> IO (Promise WorkerResponseClass)
fetchFrom = js_fetch_from . upcast

foreign import javascript unsafe "$1.fetch($2, $3)"
  js_fetch_from :: JSHandlers -> WorkerRequest -> IO (Promise WorkerResponseClass)
