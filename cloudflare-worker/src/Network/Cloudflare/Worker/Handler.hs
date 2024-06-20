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

  -- * Re-exports (for ffi exports)
  JSObject (..),
) where

import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Handler.Fetch
import Wasm.Prelude.Linear qualified as PL

newtype Handlers = Handlers {fetch :: FetchHandler}
  deriving (Generic)

type JSHandlersFields = '[ '("fetch", JSFetchHandlerClass)]

type JSHandlersClass = JSDictionaryClass JSHandlersFields

type JSHandlers = JSObject JSHandlersClass

toJSHandlers :: Handlers -> IO JSHandlers
toJSHandlers Handlers {..} = do
  let fetch' = toJSFetchHandler fetch
  reflectDictionary $
    newDictionary @JSHandlersFields
      (completeDict PL.. setPartialField @"fetch" fetch')
