{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Language.WebIDL.CodeGen.GHC.Wasm.App (defaultMain) where

import Control.Applicative ((<**>))
import qualified Data.Yaml.Aeson as Y
import Effectful (runEff)
import Effectful.Environment (runEnvironment)
import Effectful.FileSystem (runFileSystem, withCurrentDirectory)
import qualified Effectful.FileSystem as Eff
import GHC.Generics (Generic)
import Language.WebIDL.CodeGen.GHC.Wasm
import qualified Options.Applicative as Opts
import Path (File, Path, Rel, fromAbsDir, fromAbsFile, fromRelFile, parent, relfile, (</>))
import Path.IO (resolveDir', resolveFile')

newtype CLIOptions = CLIOptions {config :: FilePath}
  deriving (Show, Eq, Ord, Generic)

cliOptsP :: Opts.ParserInfo CLIOptions
cliOptsP =
  Opts.info
    (p <**> Opts.helper)
    (Opts.fullDesc <> Opts.progDesc "Generate WebAssembly bindings for WebIDL files")
  where
    p = do
      config <-
        Opts.strOption $
          mconcat
            [ Opts.long "config"
            , Opts.short 'c'
            , Opts.metavar "PATH"
            , Opts.value $ fromRelFile defaultYaml
            , Opts.showDefault
            , Opts.help "Path to the configuration yaml file"
            ]

      pure CLIOptions {..}

defaultYaml :: Path Rel File
defaultYaml = [relfile|webidl-codegen.yaml|]

defaultMain :: IO ()
defaultMain = do
  cliOpts <- Opts.execParser cliOptsP
  runEff $ runEnvironment do
    runFileSystem do
      isFile <- Eff.doesFileExist cliOpts.config
      cfgPath <-
        if isFile
          then resolveFile' cliOpts.config
          else (</> defaultYaml) <$> resolveDir' cliOpts.config
      cfg <- Y.decodeFileThrow $ fromAbsFile cfgPath
      withCurrentDirectory (fromAbsDir $ parent cfgPath) $
        generateWasmBindingWith cfg
