{-# LANGUAGE LambdaCase #-}

module Network.Cloudflare.Worker.Http (Method (..), parseMethod, formatMethod, Request (..)) where

import Control.Arrow ((>>>))
import Data.Char qualified as C
import GHC.Generics (Generic)

data Method
  = Head
  | Get
  | Post
  | Put
  | Patch
  | Delete
  | Options
  | Connect
  | Trace
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

parseMethod :: String -> Maybe Method
parseMethod =
  map C.toUpper >>> \case
    "HEAD" -> Just Head
    "GET" -> Just Get
    "POST" -> Just Post
    "PUT" -> Just Put
    "PATCH" -> Just Patch
    "DELETE" -> Just Delete
    "OPTIONS" -> Just Options
    "CONNECT" -> Just Connect
    "TRACE" -> Just Trace
    _ -> Nothing

formatMethod :: Method -> String
formatMethod Head = "HEAD"
formatMethod Get = "GET"
formatMethod Post = "POST"
formatMethod Put = "PUT"
formatMethod Patch = "PATCH"
formatMethod Delete = "DELETE"
formatMethod Options = "OPTIONS"
formatMethod Connect = "CONNECT"
formatMethod Trace = "TRACE"

data Request = Request
  { method :: !Method
  , path :: !String
  , headers :: ![(String, String)]
  }
