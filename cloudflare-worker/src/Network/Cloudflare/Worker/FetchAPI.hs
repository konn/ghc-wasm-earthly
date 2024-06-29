{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Cloudflare.Worker.FetchAPI (
  fetch,
  fetchRequest,
  requestWith,
  get,
  post,
  put,
  delete,
) where

import Data.ByteString qualified as BS
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Prim
import GHC.Wasm.Web.Generated.Request (Request)
import GHC.Wasm.Web.Generated.RequestInfo
import GHC.Wasm.Web.Generated.RequestInit
import GHC.Wasm.Web.Generated.Response (ResponseClass)
import GHC.Wasm.Web.ReadableStream (toReadableStream)
import Streaming.ByteString qualified as Q
import Wasm.Prelude.Linear qualified as PL

get :: String -> IO (Promise ResponseClass)
get uri =
  js_cf_fetch_raw (uriToReqInfo uri) none

uriToReqInfo :: String -> RequestInfo
uriToReqInfo = upcast . toUSVString . toJSString

fetchRequest :: Request -> IO (Promise ResponseClass)
fetchRequest req = js_cf_fetch_raw (inject req) none

type Method = BS.ByteString

requestWith :: Method -> String -> Maybe (Q.ByteStream IO ()) -> IO (Promise ResponseClass)
requestWith meth uri mbody = do
  rs <- mapM toReadableStream mbody
  methStr <- fromHaskellByteString meth
  let !reqInfo =
        newDictionary @RequestInitFields
          ( setPartialField "method" (nonNull methStr)
              PL.. setPartialField "body" (nonNull $ toNullable $ inject <$> rs)
          )
  js_cf_fetch_raw (uriToReqInfo uri) $ nonNull reqInfo

post :: String -> Q.ByteStream IO () -> IO (Promise ResponseClass)
post uri = requestWith "POST" uri . Just

put :: String -> Q.ByteStream IO () -> IO (Promise ResponseClass)
put uri = requestWith "PUT" uri . Just

delete :: String -> IO (Promise ResponseClass)
delete uri = requestWith "DELETE" uri Nothing

fetch :: RequestInfo -> Nullable RequestInitClass -> IO (Promise ResponseClass)
fetch = js_cf_fetch_raw

foreign import javascript safe "fetch($1, $2)"
  js_cf_fetch_raw :: RequestInfo -> Nullable RequestInitClass -> IO (Promise ResponseClass)
