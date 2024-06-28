{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Main (main, handlers) where

import Data.Aeson qualified as A
import Effectful
import GHC.Generics (Generic)
import Network.Cloudflare.Worker.Binding
import Network.Cloudflare.Worker.Handler
import Network.Cloudflare.Worker.Handler.Fetch
import Steward.Workers

foreign export javascript "handlers" handlers :: IO JSHandlers

handlers :: IO JSHandlers
handlers = toJSHandlers Handlers {fetch = fetcher}

fetcher :: FetchHandler MyBindings
fetcher = runWorker $ fromHandlers @MyBindings endpoints

type MyBindings = (BindingsClass '[] '[] '[])

data Endpoints mode = Endpoints
  { index :: mode ::: Get PlainText
  , fib :: mode ::: "fib" /> Int /> Get (JSON FibResult)
  }
  deriving (Generic)
  deriving (HasHandler (Eff es))

data FibResult = FibResult {input, result :: !Int}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (A.FromJSON, A.ToJSON)

endpoints :: Endpoints (Handler (Eff '[Worker MyBindings, IOE]))
endpoints =
  Endpoints
    { index = Handler $ pure "Hello, World!"
    , fib = Handler $ \n -> pure $ FibResult {result = fib' n, input = n}
    }

fib' :: Int -> Int
fib' n | n <= 0 = 0
fib' 1 = 1
fib' n = fib' (n - 1) + fib' (n - 2)

main :: IO ()
main = pure ()
