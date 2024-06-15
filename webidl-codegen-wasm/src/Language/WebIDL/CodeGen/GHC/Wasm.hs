{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Language.WebIDL.CodeGen.GHC.Wasm where

import Barbies
import Control.Exception.Safe (throwM)
import Control.Lens (iforM_)
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable1 (intercalate1)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
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
  (Foldable t, FileTree :> es, Reader CodeGenEnv :> es) =>
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

newtype CodeGenEnv = CodeGenEnv {modulePrefix :: Maybe T.Text}
  deriving (Generic)

toTypeName :: Identifier -> T.Text
toTypeName = id

toPrototypeName :: Identifier -> T.Text
toPrototypeName = (<> "Class")

withModulePrefix :: (Reader CodeGenEnv :> es) => NonEmpty T.Text -> Eff es T.Text
withModulePrefix names = do
  CodeGenEnv {modulePrefix} <- EffR.ask @CodeGenEnv
  pure $ maybe id (\p -> ((p <> ".") <>)) modulePrefix $ intercalate1 "." names

toMainModuleName :: (Reader CodeGenEnv :> es) => Identifier -> Eff es T.Text
toMainModuleName name = withModulePrefix $ NE.singleton name

toCoreModuleName :: (Reader CodeGenEnv :> es) => Identifier -> Eff es T.Text
toCoreModuleName names = withModulePrefix $ names NE.:| ["Core"]

generateCoreClasses ::
  ( FileTree :> es
  , Reader CodeGenEnv :> es
  ) =>
  Definitions ->
  Eff es ()
generateCoreClasses defns = do
  iforM_ defns.interfaces \name Attributed {entry = ifs} -> do
    headModule <- toCoreModuleName name
    parentMod <- mapM toCoreModuleName $ getFirst ifs.parent
    let dest = fromJust (parseRelDir $ T.unpack name) </> [relfile|Core.hs|]
        imps = "GHC.Wasm.Object.Core" : maybeToList parentMod
        proto = toPrototypeName name
        protoDef =
          [trimming|type data ${proto} :: Prototype|]
        aliasDef = [trimming|type ${name} = JSObject ${proto}|]
        superDef = case getFirst ifs.parent of
          Just p ->
            let super = toPrototypeName p
             in [trimming|type instance SuperclassOf ${proto} = 'Just ${super}|]
          Nothing -> [trimming|type instance SuperclassOf ${proto} = 'Nothing|]
        decs =
          map
            (either error id . parseDec . T.unpack)
            [protoDef, aliasDef, superDef]

    putFile dest $ formatModule $ withModuleNamed headModule imps decs
