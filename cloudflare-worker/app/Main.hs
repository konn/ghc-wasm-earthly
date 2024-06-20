{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main, handlers) where

import Data.String
import GHC.Wasm.Prim
import Network.Cloudflare.Worker.Handler
import Network.Cloudflare.Worker.Handler.Fetch (FetchHandler)
import Network.Cloudflare.Worker.Request qualified as Req
import Network.Cloudflare.Worker.Response

foreign export javascript "handlers" handlers :: IO JSHandlers

handlers :: IO JSHandlers
handlers = toJSHandlers Handlers {fetch}

fetch :: FetchHandler
fetch req _ _ = do
  url <- Req.getUrl req
  let body = "Hello, World! You requested: " <> fromString (fromJSString url)
  newResponse
    SimpleResponseInit
      { statusText = "Ok"
      , status = 200
      , headers = mempty
      , ..
      }

main :: IO ()
main = pure ()
