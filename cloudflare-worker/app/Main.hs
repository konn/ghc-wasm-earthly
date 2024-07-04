{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main, handlers) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as AKM
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict qualified as Map
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Prim
import Lucid
import Network.Cloudflare.Worker.Handler
import Network.Cloudflare.Worker.Handler.Fetch (FetchHandler)
import Network.Cloudflare.Worker.Request qualified as Req
import Network.Cloudflare.Worker.Response
import Streaming (lift)
import Streaming.ByteString qualified as Q

foreign export javascript "handlers" handlers :: IO JSHandlers

handlers :: IO JSHandlers
handlers = toJSHandlers Handlers {fetch}

fetch :: FetchHandler AnyClass
fetch req _ _ = do
  body <- fmap LT.toStrict $ renderTextT $ buildResponseBody req
  newResponse
    SimpleResponseInit
      { statusText = "Ok"
      , status = 200
      , headers = Map.fromList [("Content-Type", "text/html")]
      , ..
      }

buildResponseBody :: Req.WorkerRequest -> HtmlT IO ()
buildResponseBody req = do
  let method = toStr $ Req.getMethod req
      url = Req.getUrl req
      hdrs = Req.getHeaders req
  doctype_
  html_ $ do
    head_ $ do
      title_ "Hello Worker, From GHC, with Love!"
      link_ [rel_ "stylesheet", href_ "https://cdn.simplecss.org/simple-v1.css"]
    body_ $ do
      h1_ "Hello Worker, From GHC, with Love!"
      p_ do
        "["
        a_ [href_ "https://github.com/konn/ghc-wasm-earthly/blob/main/cloudflare-worker/app/Main.hs"] "Source Code"
        "]"
      h2_ "Info"
      p_ "Proudedly generated by GHC WASM backend."
      h2_ "Metadata"
      table_ do
        thead_ do
          tr_ do
            th_ [scope_ "col"] "Property"
            th_ [scope_ "col"] "Value"
        tbody_ do
          tr_ do
            th_ [scope_ "row"] "Method"
            td_ $ toHtml method
          tr_ do
            th_ [scope_ "row"] "Url"
            td_ $ code_ $ toHtml url
      h2_ "Headers"
      table_ do
        thead_ do
          tr_ do
            th_ [scope_ "col"] "Header"
            th_ [scope_ "col"] "Value"
        tbody_ $ forM_ hdrs \(l, r) ->
          tr_ do
            th_ [scope_ "row"] $ toHtml $ BS8.unpack l
            td_ $ code_ $ toHtml $ BS8.unpack r

      h2_ "Cloudflare Workers Specific Request Properties"
      cf <- liftIO $ Req.getCloudflareJSON req
      case cf of
        Nothing -> p_ "N/A"
        Just (J.Array xs) -> ul_ $ mapM_ (li_ . toHtml . J.encode) xs
        Just (J.Object dic) -> table_ do
          thead_ do
            tr_ do
              th_ [scope_ "col"] "Property"
              th_ [scope_ "col"] "Value"
          let dic' =
                AKM.toList $
                  AKM.delete "tlsExportedAuthenticator" $
                    AKM.delete "tlsClientExtensionsSha1" $
                      AKM.delete "tlsClientRandom" $
                        AKM.delete "tlsClientAuth" $
                          AKM.delete "botManagement" dic
          tbody_ $ forM_ dic' \(k, v) -> do
            tr_ do
              th_ [scope_ "row"] $ toHtml $ AK.toText k
              td_ $
                code_ $
                  toHtml $
                    J.encode v
        Just v -> pre_ $ code_ $ toHtml $ J.encode v
      h2_ "Body"
      body <- lift $ mapM Q.toLazy_ $ Req.readBody req
      case body of
        Nothing -> p_ "N/A"
        Just v -> pre_ $ code_ $ toHtml $ LTE.decodeUtf8 v

toStr :: (IsJavaScriptString a) => JSObject a -> String
toStr = fromJSString . convertToJSString

main :: IO ()
main = pure ()
