{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.WebIDL.CodeGen.GHC.Wasm where

import Barbies
import Control.Arrow ((>>>))
import Control.Exception.Safe (throwM)
import Control.Lens (iforM_)
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity (..))
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, maybeToList)
import Data.Monoid (First (getFirst))
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing)
import Effectful.FileSystem.IO.File (writeBinaryFile)
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as EffR
import Effectful.Writer.Static.Shared qualified as Writer
import GHC.Generics (Generic)
import GHC.Types.SrcLoc
import Language.Haskell.Parser.Ex.Helper
import Language.WebIDL.AST.Types (IDLFragment)
import Language.WebIDL.Desugar (desugar)
import Language.WebIDL.Desugar hiding (throw)
import NeatInterpolation (trimming)
import Path
import Path.IO

type GHCWasmOptions = GHCWasmOptions' (Path Abs)

data GHCWasmOptions' h = GHCWasmOptions
  { inputDir :: !(h Dir)
  , outputDir :: !(h Dir)
  , modulePrefix :: !(Maybe T.Text)
  }
  deriving (Generic)
  deriving anyclass (FunctorB, ConstraintsB, TraversableB)

deriving instance (AllBF Show h GHCWasmOptions') => Show (GHCWasmOptions' h)

deriving instance (AllBF Eq h GHCWasmOptions') => Eq (GHCWasmOptions' h)

deriving anyclass instance FromJSON (GHCWasmOptions' SomeBase)

deriving anyclass instance ToJSON (GHCWasmOptions' SomeBase)

class
  (AnyPath (SomeBase b), AbsPath (SomeBase b) ~ Path Abs b) =>
  AnyPathOver b

instance
  (AnyPath (SomeBase b), AbsPath (SomeBase b) ~ Path Abs b) =>
  AnyPathOver b

resolveOptions :: GHCWasmOptions' SomeBase -> IO GHCWasmOptions
resolveOptions = btraverseC @AnyPathOver makeAbsolute

listWebIDLs :: GHCWasmOptions -> IO [Path Abs File]
listWebIDLs GHCWasmOptions {inputDir} = do
  (_, files) <- listDirRecur inputDir
  pure $ filter ((== Just ".webidl") . fileExtension) files

data FileTree :: Effect where
  PutFile :: Path Rel File -> String -> FileTree m ()

putFile :: (FileTree :> es) => Path Rel File -> String -> Eff es ()
putFile = fmap send . PutFile

type instance DispatchOf FileTree = Dynamic

runFileTreeIOIn :: (FileSystem :> es) => Path Abs Dir -> Eff (FileTree ': es) a -> Eff es a
runFileTreeIOIn dir = interpret \cases
  _ (PutFile fp str) -> do
    let absPath = dir </> fp
    createDirectoryIfMissing True (fromAbsDir $ parent absPath)
    writeBinaryFile (fromAbsFile absPath) $ TE.encodeUtf8 $ T.pack str

runFileTreePure :: Eff (FileTree ': es) a -> Eff es (a, Map (Path Rel File) String)
runFileTreePure = reinterpret Writer.runWriter \cases
  _ (PutFile fp str) -> do
    Writer.tell $ Map.singleton fp str

execFileTreePure :: Eff (FileTree ': es) a -> Eff es (Map (Path Rel File) String)
execFileTreePure = fmap snd . runFileTreePure

generateWasmBinding ::
  (Foldable t, FileTree :> es, Reader GHCWasmOptions :> es) =>
  t IDLFragment ->
  Eff es ()
generateWasmBinding inputs = do
  defs <- either throwM pure $ desugar inputs
  generateCoreClasses defs
  pure ()

withModuleNamed :: Text -> [Text] -> [LHsDecl GhcPs] -> HsModule GhcPs
withModuleNamed name imports decls =
  HsModule
    { hsmodName = Just $ L noAnn $ ModuleName $ fromString $ T.unpack name
    , hsmodImports =
        [ imp
        | m <- imports
        , Right imp <- pure $ parseImport $ "import " <> T.unpack m
        ]
    , hsmodExt = XModulePs noAnn (EpVirtualBraces 1) Nothing Nothing
    , hsmodExports = Nothing
    , hsmodDecls = decls
    }

generateCoreClasses ::
  (FileTree :> es, Reader GHCWasmOptions :> es) =>
  Definitions ->
  Eff es ()
generateCoreClasses defns = do
  GHCWasmOptions {..} <- EffR.ask @GHCWasmOptions
  let toModuleName =
        maybe id (\p -> ((p <> ".") <>)) modulePrefix
          . T.intercalate "."
  iforM_ defns.interfaces \name Attributed {entry = ifs} -> do
    let dest = fromJust (parseRelDir $ T.unpack name) </> [relfile|Core.hs|]
        headModule = toModuleName [name]
        imps =
          "GHC.Wasm.Object.Core"
            : [ toModuleName [p, "Core"]
              | p <- maybeToList $ getFirst ifs.parent
              ]
        protoDef =
          [trimming|type data ${name} :: Prototype|]
        superDef = case getFirst ifs.parent of
          Just p -> [trimming|type instance SuperclassOf ${name} = ${p}|]
          Nothing -> [trimming|type instance SuperclassOf ${name} = AnyClass|]
        decs =
          map
            (either error id . parseDec . T.unpack)
            [protoDef, superDef]

    putFile dest $ formatModule $ withModuleNamed headModule imps decs
  pure ()
