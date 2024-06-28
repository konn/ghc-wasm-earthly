{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main, handlers) where

import Data.Function (fix, (&))
import Data.Text.Lazy qualified as LT
import Data.Vector.Unboxed.Mutable qualified as MU
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Prim
import Effectful.Random.Static (Random, evalRandom, newStdGen, uniform)
import Steward.Demo.Fib.Types
import Steward.Workers

foreign export javascript "handlers" handlers :: IO JSHandlers

handlers :: IO JSHandlers
handlers = toJSHandlers Handlers {fetch = fetcher}

type Bound = BindingsClass '[] '[] '[]

fetcher :: FetchHandler Bound
fetcher = runWorker $ do
  g <- newStdGen
  runConcurrent $ runPrim $ evalRandom g $ fromHandlers @Bound endpoints

endpoints ::
  (Random :> es, Prim :> es, Concurrent :> es) =>
  FibEndpoints (Handler (Eff es))
endpoints =
  FibEndpoints
    { random = Handler serveRandom
    , index = Handler indexPage
    , fib = Handler serveFib
    }

serveFib :: (Prim :> es) => Int -> Eff es FibResult
serveFib n = do
  mv <- MU.new $ n + 1
  MU.write mv 0 0
  MU.write mv 1 1
  2 & fix \self !i ->
    if i > n
      then pure ()
      else do
        k <- MU.read mv $ i - 2
        l <- MU.read mv $ i - 1
        MU.write mv i $ k + l
        self $ i + 1
  result <- MU.read mv n
  pure FibResult {input = n, ..}

indexPage :: Eff es LT.Text
indexPage = pure "Hello, I'm a worker!"

serveRandom :: (Concurrent :> es, Random :> es) => Eff es Int
serveRandom = uniform

main :: IO ()
main = pure ()
