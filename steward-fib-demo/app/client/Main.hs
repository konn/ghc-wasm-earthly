{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Applicative ((<**>))
import Effectful
import GHC.Generics (Generic)
import Options.Applicative qualified as Opt
import Steward.Client.Effectful (StewardClient, URI, parseURI, runStewardClient)
import Steward.Demo.Fib.Types
import Steward.Types

data Opts = Opts {endpoint :: !URI, command :: !Command}
  deriving (Show, Eq, Ord, Generic)

data Command = Top | Fib !Int | Random
  deriving (Show, Eq, Ord, Generic)

optsP :: Opt.ParserInfo Opts
optsP = Opt.info (parser <**> Opt.helper) (Opt.fullDesc <> Opt.progDesc "Steward Fib Demo")
  where
    parser = do
      endpoint <-
        Opt.option
          (Opt.maybeReader parseURI)
          (Opt.long "endpoint" <> Opt.metavar "URI" <> Opt.help "Steward endpoint")
      command <-
        Opt.hsubparser $
          mconcat
            [ Opt.command "top" (Opt.info (pure Top) (Opt.progDesc "Get top"))
            , Opt.command "fib" (Opt.info (Fib <$> Opt.argument Opt.auto (Opt.metavar "N")) (Opt.progDesc "Get Fibonacci number"))
            , Opt.command "random" (Opt.info (pure Random) (Opt.progDesc "Get random number"))
            ]
      pure Opts {..}

fibs ::
  (StewardClient :> es) =>
  FibEndpoints (Client (Eff es))
fibs = client

main :: IO ()
main = do
  opts <- Opt.customExecParser (Opt.prefs Opt.subparserInline) optsP
  runEff $ runStewardClient opts.endpoint do
    liftIO $ putStrLn $ "Running command: " <> show opts.command
    liftIO $ putStrLn $ "Endpoint: " <> show opts.endpoint

    case opts.command of
      Top -> do
        res <- fibs.index.call
        liftIO $ print res
      Fib n -> do
        res <- fibs.fib.call n
        liftIO $ print res
      Random -> do
        res <- fibs.random.call
        liftIO $ print res
