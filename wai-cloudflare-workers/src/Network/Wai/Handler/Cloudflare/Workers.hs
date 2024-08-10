{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Network.Wai.Handler.Cloudflare.Workers (run) where

import Control.Concurrent.MVar
import Control.Exception.Safe (Exception (displayException), tryAny)
import qualified Data.Bifunctor as Bi
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import qualified Data.Char as C
import Data.Generics.Labels ()
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Vault.Lazy (Key, newKey)
import qualified Data.Vault.Lazy as Vault
import GHC.IsList (IsList (..))
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.ReadableStream (ReadableStream)
import qualified GHC.Wasm.Web.ReadableStream as RS
import Network.Cloudflare.Worker.Handler.Fetch
import Network.Cloudflare.Worker.Request (WorkerIncomingRequestCf)
import qualified Network.Cloudflare.Worker.Request as Req
import qualified Network.Cloudflare.Worker.Response as Resp
import Network.HTTP.Types.URI (decodePathSegments, parseQuery)
import Network.HTTP.Types.Version
import Network.Socket (SockAddr (..), tupleToHostAddress, tupleToHostAddress6)
import Network.URI (URI (..), parseURI)
import Network.Wai
import Network.Wai.Internal (Request (..), ResponseReceived (..))
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

run :: Application -> FetchHandler e
run app cfReq env ctx = do
  respVar <- newEmptyMVar
  req' <- toWaiReq env ctx cfReq
  eith <- tryAny $ app req' \resp -> do
    putMVar respVar =<< fromWaiResp resp
    pure ResponseReceived
  case eith of
    Right ResponseReceived -> takeMVar respVar
    Left e -> do
      Resp.newResponse
        Resp.SimpleResponseInit
          { body = "Exception: " <> T.pack (displayException e)
          , status = 500
          , statusText = "Internal Server Error"
          , headers = fromList [("Content-Type", "text/plain")]
          }

fromWaiResp :: Response -> IO Resp.WorkerResponse
fromWaiResp = undefined

toWaiReq :: JSObject e -> FetchContext -> Req.WorkerRequest -> IO Request
toWaiReq e ctx wreq = do
  let cf = Req.getCloudflare wreq
  requestMethod <- toHaskellByteString $ Req.getMethod wreq
  proto <- toText <$> getDictField "httpProtocol" cf
  let requestHeaders = map (Bi.first CI.mk) $ Req.getHeaders wreq
      uri = fromMaybe (error "Invalid URI") $ parseURI $ T.unpack $ Req.getUrl wreq
      rawPathInfo = BS8.pack $ uriPath uri
      pathInfo = decodePathSegments rawPathInfo
      rawQueryString = BS8.pack $ uriQuery uri
      queryString = parseQuery rawQueryString
      !hdrs = HM.fromList requestHeaders
      requestHeaderHost = HM.lookup "Host" hdrs
      requestHeaderRange = HM.lookup "Range" hdrs
      requestHeaderReferer = HM.lookup "Referer" hdrs
      requestHeaderUserAgent = HM.lookup "User-Agent" hdrs
      remoteHost =
        maybe (SockAddrInet 0 $ tupleToHostAddress (0, 0, 0, 0)) parseIP $
          HM.lookup "CF-Connecting-IP" hdrs
      isSecure = True
      httpVersion = parseProto proto
      vault =
        Vault.insert cloudflareMetaKey cf $
          Vault.insert cloudflareContextKey ctx $
            Vault.insert cloudflareEnvKey e Vault.empty
  (requestBody, requestBodyLength) <-
    maybe
      (pure (pure "", KnownLength 0))
      fromWorkerBody
      $ fromNullable
      $ Req.getBody wreq
  pure Request {..}

fromWorkerBody :: ReadableStream -> IO (IO BS8.ByteString, RequestBodyLength)
fromWorkerBody rstr = do
  reader <- RS.getReader rstr
  let pop = fromMaybe BS8.empty <$> RS.popReader reader
  pure (pop, ChunkedBody)

parseIP :: BS8.ByteString -> SockAddr
parseIP ip
  | Just [a, b, c, d] <- mapM (readMaybe . BS8.unpack) $ BS8.split '.' ip =
      SockAddrInet 0 $ tupleToHostAddress (a, b, c, d)
  | Just [a, b, c, d, e, f, g, h] <-
      mapM (readMaybe . (\x -> if null x then "0" else x) . BS8.unpack) $ BS8.split ':' ip =
      SockAddrInet6 0 0 (tupleToHostAddress6 (a, b, c, d, e, f, g, h)) 0
  | otherwise = SockAddrUnix $ BS8.unpack ip

parseProto :: T.Text -> HttpVersion
parseProto (T.toUpper -> proto) = fromMaybe http11 do
  vstr <- T.stripPrefix "HTTP/" proto
  let (majStr, rest) = T.break (not . C.isDigit) vstr
  maj <- readMaybe $ T.unpack majStr
  minr <-
    if T.null rest
      then pure 0
      else do
        readMaybe . T.unpack =<< T.stripPrefix "." rest
  pure HttpVersion {httpMajor = maj, httpMinor = minr}

cloudflareEnvKey :: Key (JSObject e)
{-# NOINLINE cloudflareEnvKey #-}
cloudflareEnvKey = unsafePerformIO newKey

cloudflareContextKey :: Key FetchContext
{-# NOINLINE cloudflareContextKey #-}
cloudflareContextKey = unsafePerformIO newKey

cloudflareMetaKey :: Key WorkerIncomingRequestCf
{-# NOINLINE cloudflareMetaKey #-}
cloudflareMetaKey = unsafePerformIO newKey
