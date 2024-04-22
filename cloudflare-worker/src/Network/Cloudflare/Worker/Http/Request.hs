{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Network.Cloudflare.Worker.Http.Request (
  Request (..),
  RequestOptions (..),
  MinifyOptions (..),
) where

import Data.Aeson.Micro
import Data.Aeson.Micro.Generics ()
import Data.Map.Strict (Map)
import Data.Text qualified as T
import GHC.Generics
import GHC.Wasm.Prim

newtype Request = Request JSVal

data RequestInitCfProperties = RequestInitCfProperties
  { apps :: Maybe Bool
  , cacheEverything :: Maybe Bool
  , cacheKey :: Maybe T.Text
  , cacheTags :: Maybe [T.Text]
  , cacheTtl :: Maybe Int
  , cacheTtlByStatus :: Maybe (Map T.Text Int)
  , image :: Maybe Value
  , minify :: Maybe MinifyOptions
  , mirage :: Maybe Bool
  , polish :: Maybe T.Text
  , resolveOverride :: Maybe T.Text
  , scrapeShield :: Maybe Bool
  , webp :: Maybe Bool
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Generically RequestInitCfProperties

data MinifyOptions = MinifyOptions
  { javascript :: Maybe Bool
  , css :: Maybe Bool
  , html :: Maybe Bool
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Generically MinifyOptions

data RequestOptions = RequestOptions
  { cf :: Maybe RequestInitCfProperties
  , method :: Maybe T.Text
  , headers :: Maybe (Map T.Text T.Text)
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Generically RequestOptions
