{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.WebIDL.CodeGen.GHC.Wasm (
  GHCWasmOptions' (..),
  generateWasmBinding,
  generateWasmBindingPure,
  generateWasmBindingWith,
  runFileTreePure,
  execFileTreePure,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Control.Arrow ((>>>))
import Control.Exception.Safe (throwIO, throwString)
import Control.Foldl qualified as L
import Control.Lens (ifoldMap, imapM_, ix, (%~))
import Control.Lens.Indexed (iforM_)
import Control.Monad (when, (<=<))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor qualified as Bi
import Data.ByteString qualified as BS
import Data.Char qualified as C
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Data (Data)
import Data.Foldable1 (intercalate1)
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (..))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, fromMaybe, isNothing, maybeToList)
import Data.Monoid (First (getFirst))
import Data.Scientific (floatingOrInteger)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Vector qualified as V
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Effectful.Dispatch.Dynamic
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing)
import Effectful.FileSystem.IO (hFlush, stderr)
import Effectful.FileSystem.IO.ByteString (hPutStrLn)
import Effectful.FileSystem.IO.ByteString qualified as Eff
import Effectful.FileSystem.IO.File (writeBinaryFile)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Reader.Static qualified as EffR
import Effectful.Time (Clock, getZonedTime)
import Effectful.Writer.Static.Local (execWriter)
import Effectful.Writer.Static.Local qualified as Writer
import Effectful.Writer.Static.Shared qualified as SWriter
import GHC.Generics (Generic, Generically (..))
import GHC.Types.Name
import GHC.Types.Name.Reader (RdrName (..))
import GHC.Types.SrcLoc
import Language.Haskell.Parser.Ex.Helper
import Language.WebIDL.AST.Parser (parseIDLFragment)
import Language.WebIDL.AST.Types (Attribute (..), AttributeName (..), BufferType (..), ConstType (..), ConstValue (..), DistinguishableType (..), IDLFragment, PrimType (..), Sign (..), StringType (..), UnionType (..), WithNullarity (..))
import Language.WebIDL.Desugar hiding (throw)
import NeatInterpolation (trimming)
import Path
import Path.IO
import Text.Megaparsec.Error qualified as P

type GHCWasmOptions = GHCWasmOptions' (Path Abs Dir)

data GHCWasmOptions' fp = GHCWasmOptions
  { inputDir :: !fp
  , outputDir :: !fp
  , modulePrefix :: !(Maybe T.Text)
  , targets :: Maybe [Text]
  , predefinedTypes :: Maybe [Text]
  , extraImports :: Maybe [Text]
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (FromJSON, ToJSON)

data Message :: Effect where
  PutLog :: Text -> Message m ()

type instance DispatchOf Message = Dynamic

putLog :: (Message :> es) => Text -> Eff es ()
putLog = send . PutLog

ignoreMessages :: Eff (Message ': es) a -> Eff es a
ignoreMessages = interpret \cases
  _ PutLog {} -> pure ()

runMessageStdout :: (Console :> es, Clock :> es) => Eff (Message ': es) a -> Eff es a
runMessageStdout = interpret \cases
  _ (PutLog msg) -> Console.putStrLn =<< formatMsg msg

runMessageStderr :: (FileSystem :> es, Clock :> es) => Eff (Message ': es) a -> Eff es a
runMessageStderr = interpret \cases
  _ (PutLog msg) -> do
    hPutStrLn stderr =<< formatMsg msg
    hFlush stderr

formatMsg :: (Clock :> es) => Text -> Eff es BS.ByteString
formatMsg msg = do
  now <- getZonedTime
  let time = T.pack $ formatTime defaultTimeLocale "[%Y-%m-%d %H:%M:%S] " now
  pure $ TE.encodeUtf8 $ time <> msg

resolveOptions :: (FileSystem :> es) => GHCWasmOptions' FilePath -> Eff es GHCWasmOptions
resolveOptions = traverse (unsafeEff_ . resolveDir')

listWebIDLs :: (FileSystem :> es) => GHCWasmOptions -> Eff es [Path Abs File]
listWebIDLs GHCWasmOptions {inputDir} = do
  (_, files) <- unsafeEff_ $ listDirRecur inputDir
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
runFileTreePure = reinterpret SWriter.runWriter \cases
  _ (PutFile fp str) -> do
    SWriter.tell $ Map.singleton fp str

execFileTreePure :: Eff (FileTree ': es) a -> Eff es (Map (Path Rel File) String)
execFileTreePure = fmap snd . runFileTreePure

buildCodeGenEnv ::
  GHCWasmOptions' fp ->
  Definitions ->
  CodeGenEnv
buildCodeGenEnv opts definitions =
  let !unknownTypes = fromMaybe mempty $ getUnknownIdentifiers definitions
      !dependencies = getDependencies definitions
      !modulePrefix = opts.modulePrefix
      !targets =
        opts.targets <&> \ts ->
          L.fold L.hashSet $ foldMap (`AM.postSet` dependencies) ts
      !predefinedTypes = foldMap Set.fromList opts.predefinedTypes
      !extraImports = foldMap Set.fromList opts.extraImports
   in CodeGenEnv {..}

skipNonTarget :: (Reader CodeGenEnv :> es, Message :> es) => Identifier -> Eff es () -> Eff es ()
skipNonTarget name act = do
  targets <- EffR.asks @CodeGenEnv (.targets)
  case targets of
    Just ts | not $ HS.member name ts -> pure () -- putLog $ "Skipping: " <> name
    _ -> act

getDependencies :: Definitions -> AM.AdjacencyMap Identifier
getDependencies defs =
  AM.reflexiveClosure $
    AM.transitiveClosure $
      mconcat
        [ ifoldMap
            ( \f ->
                foldMap (AM.edge f)
                  . L.foldOver namedTypeIdentifiers L.nub
            )
            defs.interfaces
        , ifoldMap
            ( \f ->
                foldMap (AM.edge f)
                  . L.foldOver namedTypeIdentifiers L.nub
            )
            defs.mixins
        , ifoldMap
            ( \f ->
                foldMap (AM.edge f)
                  . L.foldOver namedTypeIdentifiers L.nub
            )
            defs.typedefs
        , ifoldMap
            ( \f ->
                foldMap (AM.edge f)
                  . L.foldOver namedTypeIdentifiers L.nub
            )
            defs.enums
        , ifoldMap
            ( \f ->
                foldMap (AM.edge f)
                  . L.foldOver namedTypeIdentifiers L.nub
            )
            defs.dictionaries
        , ifoldMap
            ( \f ->
                foldMap (AM.edge f)
                  . L.foldOver namedTypeIdentifiers L.nub
            )
            defs.callbackFunctions
        , ifoldMap
            ( \f ->
                foldMap (AM.edge f)
                  . L.foldOver namedTypeIdentifiers L.nub
            )
            defs.callbackInterfaces
        ]

generateWasmBindingWith ::
  (FileSystem :> es, Clock :> es) =>
  GHCWasmOptions' FilePath ->
  Eff es ()
generateWasmBindingWith opts = do
  opts' <- resolveOptions opts
  idls <-
    mapM
      ( either (throwString . P.errorBundlePretty) pure . parseIDLFragment . TE.decodeUtf8
          <=< Eff.readFile . fromAbsFile
      )
      =<< listWebIDLs opts'
  !defs <- either throwIO pure $ desugar idls
  runMessageStderr $
    runFileTreeIOIn opts'.outputDir $
      runReader (buildCodeGenEnv opts' defs) generateWasmBinding

generateWasmBindingPure ::
  (Foldable t, FileSystem :> es) =>
  GHCWasmOptions' fp ->
  t IDLFragment ->
  Eff es (Map (Path Rel File) String)
generateWasmBindingPure opts idls = do
  !defs <- either throwIO pure $ desugar idls
  execFileTreePure $
    ignoreMessages $
      runReader (buildCodeGenEnv opts defs) generateWasmBinding

data CoreModule = CoreModule
  { exports :: [Text]
  , decls :: [Either Text (HsDecl GhcPs)]
  }
  deriving (Generic)

-- FIXME: Create just single one Core module to host all prototypes,
-- and import them in all other generated modules.
generateWasmBinding ::
  (FileTree :> es, Reader CodeGenEnv :> es, Message :> es) =>
  Eff es ()
generateWasmBinding = do
  CodeGenEnv {..} <- EffR.ask
  case targets of
    Just ts -> putLog $ "Generating WebIDL bindings for " <> T.pack (show ts)
    Nothing -> putLog "Generating WebIDL bindings for all types"
  generateDictionaryModules
  generateEnumModules
  generateTypedefModules
  generateCallbackFunctionModules
  generateCallbackInterfaceModules
  generateInterfaceCoreModules
  generateInterfaceMainModules

data Module = Module
  { moduleName :: Text
  , exports :: Maybe [(NameSpace, Text)]
  , imports :: [Text]
  , decls :: [Either Text (HsDecl GhcPs)]
  }
  deriving (Generic)

renderModule :: Module -> Either String String
renderModule Module {..} = do
  hsmodImports <-
    mapM
      (either Left pure . parseImport . ("import " <>) . T.unpack)
      imports
  hsmodDecls <-
    mapM
      (either (either Left Right . parseDec . T.unpack) (Right . L noAnn))
      decls
  pure $
    formatModule $
      HsModule
        { hsmodName = Just $ L noAnn $ ModuleName $ fromString $ T.unpack moduleName
        , hsmodImports
        , hsmodExt = XModulePs noAnn (EpVirtualBraces 1) Nothing Nothing
        , hsmodExports =
            L noAnn
              . map
                (\(ns, nam) -> L noAnn $ IEVar Nothing (L noAnn (IEName noExtField $ L noAnn $ Unqual $ mkOccName ns $ T.unpack nam)) Nothing)
              <$> exports
        , hsmodDecls
        }

data CodeGenEnv = CodeGenEnv
  { modulePrefix :: !(Maybe T.Text)
  , unknownTypes :: !(Set Identifier)
  , dependencies :: !(AM.AdjacencyMap Identifier)
  , definitions :: !Definitions
  , targets :: !(Maybe (HashSet Identifier))
  , predefinedTypes :: Set Text
  , extraImports :: Set Text
  }
  deriving (Generic)

toTypeName :: Identifier -> T.Text
toTypeName = normaliseTypeName

toPrototypeName :: Identifier -> T.Text
toPrototypeName = (<> "Class") . normaliseTypeName

withModulePrefix :: (Reader CodeGenEnv :> es) => NonEmpty T.Text -> Eff es T.Text
withModulePrefix names = do
  CodeGenEnv {modulePrefix} <- EffR.ask @CodeGenEnv
  pure $ maybe id (\p -> ((p <> ".") <>)) modulePrefix $ intercalate1 "." names

toMainModuleName :: (Reader CodeGenEnv :> es) => Identifier -> Eff es T.Text
toMainModuleName name = withModulePrefix $ NE.singleton $ normaliseTypeName name

toCoreModuleName :: (Reader CodeGenEnv :> es) => Identifier -> Eff es T.Text
toCoreModuleName names = withModulePrefix $ normaliseTypeName names NE.:| ["Core"]

generateInterfaceCoreModules ::
  ( FileTree :> es
  , Reader CodeGenEnv :> es
  , Message :> es
  ) =>
  Eff es ()
generateInterfaceCoreModules = do
  putLog "* Generating interface core modules..."
  defns <- EffR.asks @CodeGenEnv (.definitions)
  imapM_ generateInterfaceCoreModule defns.interfaces

generateInterfaceMainModules ::
  ( FileTree :> es
  , Reader CodeGenEnv :> es
  , Message :> es
  ) =>
  Eff es ()
generateInterfaceMainModules = do
  putLog "* Generating interface Main modules..."
  defns <- EffR.asks @CodeGenEnv (.definitions)
  imapM_ generateInterfaceMainModule defns.interfaces

generateInterfaceCoreModule ::
  ( FileTree :> es
  , Reader CodeGenEnv :> es
  , Message :> es
  ) =>
  Text ->
  Attributed Interface ->
  Eff es ()
generateInterfaceCoreModule (normaliseTypeName -> name) Attributed {entry = ifs} = skipNonTarget name do
  moduleName <- toCoreModuleName name
  putLog $ "Generating: " <> moduleName
  parentMod <- mapM (toCoreModuleName . (.typeName)) ifs.parent.getFirst
  extras <- EffR.asks @CodeGenEnv (.extraImports)
  let dest = fromJust (parseRelDir $ T.unpack name) </> [relfile|Core.hs|]
      imports = Set.toList $ Set.fromList (presetImports ++ maybeToList parentMod) <> extras
      proto = toPrototypeName name
      protoDef =
        [trimming|type data ${proto} :: Prototype|]
      aliasDef = [trimming|type ${name} = JSObject ${proto}|]
      superDef = case ifs.parent.getFirst of
        Just p ->
          let super = toPrototypeName p.typeName
           in [trimming|type instance SuperclassOf ${proto} = 'Just ${super}|]
        Nothing -> [trimming|type instance SuperclassOf ${proto} = 'Nothing|]
      decls = map Left [protoDef, aliasDef, superDef]
      exports = Just [(tvName, proto), (tvName, name)]

  either throwString (putFile dest) $ renderModule Module {..}

data MainModuleFragments = MainModuleFragments
  { mainExports :: DList (NameSpace, Text)
  , decs :: DList (Either Text (HsDecl GhcPs))
  }
  deriving (Generic)
  deriving (Semigroup, Monoid) via Generically MainModuleFragments

getParents :: (Data a, Reader CodeGenEnv :> es) => a -> Eff es (Set Identifier)
getParents x = do
  predefs <- EffR.asks @CodeGenEnv (.predefinedTypes)
  pure $ L.foldOver namedTypeIdentifiers L.set x Set.\\ predefs

generateDictionaryModules ::
  ( FileTree :> es
  , Reader CodeGenEnv :> es
  , Message :> es
  ) =>
  Eff es ()
generateDictionaryModules = do
  putLog "* Generating dictionary modules..."
  defns <- EffR.asks @CodeGenEnv (.definitions)
  iforM_ defns.dictionaries \(normaliseTypeName -> name) Attributed {entry = dict} -> skipNonTarget name do
    coreModuleName <- toCoreModuleName name
    mainModuleName <- toMainModuleName name
    putLog $ "Generating: " <> T.pack (show (coreModuleName, mainModuleName))
    parents <- getParents dict
    imps <- mapM toCoreModuleName $ Set.toList parents
    extras <- EffR.asks @CodeGenEnv (.extraImports)
    let proto = toPrototypeName name
        coreDest = fromJust (parseRelDir $ T.unpack name) </> [relfile|Core.hs|]
        dest = fromJust (parseRelFile $ T.unpack name <> ".hs")
        imports = Set.toList $ Set.fromList imps <> Set.fromList presetImports <> extras
        fieldTy =
          promotedListTy $
            map (uncurry promotedTupleT . Bi.first symbolLitTy) $
              Map.toList $
                (toHaskellPrototype . (.entry) <$> dict.requiredMembers)
                  <> (appTy (tyConOrVar "NullableClass") . toHaskellPrototype . fst . (.entry) <$> dict.optionalMembers)
        fieldTyStr = T.pack $ pprint fieldTy
        reifiedName = "Reified" <> name
        field = name <> "Fields"
        fieldsDef =
          [trimming|
            type ${field} = ${fieldTyStr}
          |]
        protoDef =
          [trimming|
            type ${proto} = JSDictionaryClass ${field}
          |]
        alias =
          [trimming|
            type ${name} = JSObject ${proto}
          |]
        reified =
          [trimming|
            type ${reifiedName} = ReifiedDictionary ${field}
          |]

        exports = Just [(tvName, field), (tvName, proto), (tvName, name), (tvName, reifiedName)]
        decls = map Left [fieldsDef, protoDef, alias, reified]
    either throwString (putFile coreDest) $
      renderModule Module {moduleName = coreModuleName, ..}
    either throwString (putFile dest) $
      renderModule
        Module
          { moduleName = mainModuleName
          , imports = coreModuleName : Set.toList extras
          , exports = exports
          , decls = []
          }

generateEnumModules ::
  ( FileTree :> es
  , Reader CodeGenEnv :> es
  , Message :> es
  ) =>
  Eff es ()
generateEnumModules = do
  putLog "* Generating enum modules..."
  defns <- EffR.asks @CodeGenEnv (.definitions)
  iforM_ defns.enums \(normaliseTypeName -> name) Attributed {entry = enum} -> skipNonTarget name do
    coreModuleName <- toCoreModuleName name
    mainModuleName <- toMainModuleName name
    putLog $ "Generating: " <> T.pack (show (coreModuleName, mainModuleName))
    parents <- getParents enum
    extras <- EffR.asks @CodeGenEnv (.extraImports)
    imps <- mapM toCoreModuleName $ Set.toList parents
    let proto = toPrototypeName name
        coreDest = fromJust (parseRelDir $ T.unpack name) </> [relfile|Core.hs|]
        dest = fromJust (parseRelFile $ T.unpack name <> ".hs")
        imports = Set.toList $ Set.fromList imps <> Set.fromList presetImports <> extras
        tagsTy =
          promotedListTy $
            map symbolLitTy $
              NE.toList enum.enumTags
        tagsTyStr = T.pack $ pprint tagsTy
        field = name <> "Tags"
        fieldsDef =
          [trimming|
            type ${field} = ${tagsTyStr}
          |]
        protoDef =
          [trimming|
            type ${proto} = EnumClass ${field}
          |]
        alias =
          [trimming|
            type ${name} = JSObject ${proto}
          |]

        exports = Just [(tvName, field), (tvName, proto), (tvName, name)]
        decls = map Left [fieldsDef, protoDef, alias]
    either throwString (putFile coreDest) $
      renderModule Module {moduleName = coreModuleName, ..}
    either throwString (putFile dest) $
      renderModule
        Module
          { moduleName = mainModuleName
          , imports = coreModuleName : Set.toList extras
          , exports = exports
          , decls = []
          }

generateCallbackInterfaceModules ::
  ( FileTree :> es
  , Reader CodeGenEnv :> es
  , Message :> es
  ) =>
  Eff es ()
generateCallbackInterfaceModules = do
  putLog "* Generating callback interface modules..."
  defns <- EffR.asks @CodeGenEnv (.definitions)
  iforM_ defns.callbackInterfaces \(normaliseTypeName -> name) Attributed {entry = callback} -> skipNonTarget name do
    coreModuleName <- toCoreModuleName name
    mainModuleName <- toMainModuleName name
    putLog $ "Generating: " <> T.pack (show (coreModuleName, mainModuleName))
    parents <- Set.toList <$> getParents callback
    imps <- mapM toCoreModuleName parents
    extras <- EffR.asks @CodeGenEnv (.extraImports)
    MainModuleFragments {mainExports = constExports, decs = constDefs} <-
      Writer.execWriter $
        V.mapM (generateConstantFragments name . (.entry)) callback.callbackConsts
    let proto = toPrototypeName name
        coreDest = fromJust (parseRelDir $ T.unpack name) </> [relfile|Core.hs|]
        dest = fromJust (parseRelFile $ T.unpack name <> ".hs")
        imports = Set.toList $ Set.fromList imps <> Set.fromList presetImports <> extras
        protoDef =
          [trimming|
            type data ${proto} :: Prototype
          |]
        superDef =
          [trimming|type instance SuperclassOf ${proto} = 'Nothing|]
        alias =
          [trimming|
            type ${name} = JSObject ${proto}
          |]
        argTys = argumentHsTypes callback.callbackOperation.args
        pureName = "js_mk_callback_" <> name <> "_pure"
        pureArgTy =
          foldr mkNormalFunTy (toHaskellType callback.callbackOperation.returnType) argTys
        pureTy = T.pack $ pprint $ pureArgTy `mkNormalFunTy` tyConOrVar name
        pureFFI =
          [trimming|
            foreign import javascript unsafe "wrapper"
              ${pureName} :: ${pureTy}
          |]
        impureName = "js_mk_callback_" <> name <> "_impure"
        impureArgTy =
          foldr mkNormalFunTy (ioTy `appTy` toHaskellType callback.callbackOperation.returnType) argTys
        impureTy = T.pack $ pprint $ impureArgTy `mkNormalFunTy` tyConOrVar name
        impureFFI =
          [trimming|
            foreign import javascript unsafe "wrapper"
              ${impureName} :: ${impureTy}
          |]

        exports =
          Just $
            (tvName, proto)
              : (tvName, name)
              : (varName, pureName)
              : (varName, impureName)
              : DL.toList constExports
        decls =
          Left protoDef
            : Left superDef
            : Left alias
            : Left pureFFI
            : Left impureFFI
            : DL.toList constDefs
    either throwString (putFile coreDest) $
      renderModule Module {moduleName = coreModuleName, ..}
    either throwString (putFile dest) $
      renderModule
        Module
          { moduleName = mainModuleName
          , imports = coreModuleName : Set.toList extras
          , exports = exports
          , decls = []
          }

generateCallbackFunctionModules ::
  ( FileTree :> es
  , Reader CodeGenEnv :> es
  , Message :> es
  ) =>
  Eff es ()
generateCallbackFunctionModules = do
  putLog "* Generating callback function modules..."
  defns <- EffR.asks @CodeGenEnv (.definitions)
  iforM_ defns.callbackFunctions \(normaliseTypeName -> name) Attributed {entry = callback} -> skipNonTarget name do
    coreModuleName <- toCoreModuleName name
    mainModuleName <- toMainModuleName name
    putLog $ "Generating: " <> T.pack (show (coreModuleName, mainModuleName))
    parents <- Set.toList <$> getParents callback
    imps <- mapM toCoreModuleName parents
    extras <- EffR.asks @CodeGenEnv (.extraImports)
    let proto = toPrototypeName name
        coreDest = fromJust (parseRelDir $ T.unpack name) </> [relfile|Core.hs|]
        dest = fromJust (parseRelFile $ T.unpack name <> ".hs")
        imports = Set.toList $ Set.fromList imps <> Set.fromList presetImports <> extras
        protoDef =
          [trimming|
            type data ${proto} :: Prototype
          |]
        superDef =
          [trimming|type instance SuperclassOf ${proto} = 'Nothing|]
        alias =
          [trimming|
            type ${name} = JSObject ${proto}
          |]
        argTys = argumentHsTypes callback.argTypes
        pureName = "js_mk_callback_" <> name <> "_pure"
        pureArgTy =
          foldr mkNormalFunTy (toHaskellType callback.returnType) argTys
        pureTy = T.pack $ pprint $ pureArgTy `mkNormalFunTy` tyConOrVar name
        pureFFI =
          [trimming|
            foreign import javascript unsafe "wrapper"
              ${pureName} :: ${pureTy}
          |]
        impureName = "js_mk_callback_" <> name <> "_impure"
        impureArgTy =
          foldr mkNormalFunTy (ioTy `appTy` toHaskellType callback.returnType) argTys
        impureTy = T.pack $ pprint $ impureArgTy `mkNormalFunTy` tyConOrVar name
        impureFFI =
          [trimming|
            foreign import javascript unsafe "wrapper"
              ${impureName} :: ${impureTy}
          |]

        exports = Just [(tvName, proto), (tvName, name), (varName, pureName), (varName, impureName)]
        decls = map Left [protoDef, superDef, alias, pureFFI, impureFFI]
    either throwString (putFile coreDest) $
      renderModule Module {moduleName = coreModuleName, ..}
    either throwString (putFile dest) $
      renderModule
        Module
          { moduleName = mainModuleName
          , imports = coreModuleName : Set.toList extras
          , exports = exports
          , decls = []
          }

generateTypedefModules ::
  ( FileTree :> es
  , Reader CodeGenEnv :> es
  , Message :> es
  ) =>
  Eff es ()
generateTypedefModules = do
  putLog "* Generating typedef modules..."
  defns <- EffR.asks @CodeGenEnv (.definitions)
  iforM_ defns.typedefs \(normaliseTypeName -> name) Attributed {entry = typedef} -> skipNonTarget name do
    coreModuleName <- toCoreModuleName name
    mainModuleName <- toMainModuleName name
    putLog $ "Generating: " <> T.pack (show (coreModuleName, mainModuleName))
    parents <- Set.toList <$> getParents typedef
    imps <- mapM toCoreModuleName parents
    extras <- EffR.asks @CodeGenEnv (.extraImports)
    let proto = toPrototypeName name
        coreDest = fromJust (parseRelDir $ T.unpack name) </> [relfile|Core.hs|]
        dest = fromJust (parseRelFile $ T.unpack name <> ".hs")
        imports = Set.toList $ Set.fromList imps <> Set.fromList presetImports <> extras
        protoTy = T.pack $ pprint $ toHaskellPrototype typedef.entry
        aliasTy = T.pack $ pprint $ toHaskellType typedef.entry
        protoDef =
          [trimming|
            type ${proto} = ${protoTy}
          |]
        alias =
          [trimming|
            type ${name} = ${aliasTy}
          |]

        exports = Just [(tvName, proto), (tvName, name)]
        decls = map Left [protoDef, alias]
    either throwString (putFile coreDest) $
      renderModule Module {moduleName = coreModuleName, ..}
    either throwString (putFile dest) $
      renderModule
        Module
          { moduleName = mainModuleName
          , imports = coreModuleName : Set.toList extras
          , exports = exports
          , decls = []
          }

generateInterfaceMainModule ::
  forall es.
  ( FileTree :> es
  , Reader CodeGenEnv :> es
  , Message :> es
  ) =>
  Text ->
  Attributed Interface ->
  Eff es ()
generateInterfaceMainModule name Attributed {entry = ifs} = skipNonTarget name do
  moduleName <- toMainModuleName hsTyName
  putLog $ "Generating: " <> moduleName
  let dest = fromJust (parseRelFile $ T.unpack hsTyName <> ".hs")
  idents <- Set.toList <$> getParents ifs
  coreMod <- toCoreModuleName hsTyName
  parentMods <- mapM toCoreModuleName idents
  extras <- EffR.asks @CodeGenEnv (.extraImports)
  MainModuleFragments {..} <- execWriter go
  let imports =
        Set.toList $
          Set.fromList (coreMod : presetImports ++ parentMods) <> extras
      proto = toPrototypeName hsTyName
      exports =
        Just $
          (tvName, hsTyName)
            : (tvName, proto)
            : DL.toList mainExports
      decls = DL.toList decs
  either throwString (putFile dest) $
    renderModule Module {..}
  where
    hsTyName = normaliseTypeName name
    go = do
      genConstructors
      genConstants
      genOperations
      genStringifiers
      genAttributes
      genIterables
      genAsyncIterables
      genStaticAttributes
      genStaticOperations
    -- FIXME: Is this correct? We must be aware of 'Namespace'.
    genConstructors = V.forM_ ifs.constructors \Attributed {entry = args} -> do
      let hsFunName
            | V.length ifs.constructors /= 1 = toConstructorName hsTyName args
            | otherwise = "js_cons_" <> hsTyName
          jsFun = Call hsTyName
          returnType = ioTy `appTy` tyConOrVar (toTypeName hsTyName)
          ffiDec = renderJSFFIImport $ toJSFFIImport JSFFIImportSeed {async = False, ..}
      Writer.tell
        MainModuleFragments
          { mainExports = DL.singleton (varName, hsFunName)
          , decs = DL.singleton ffiDec
          }

    genConstants = V.forM_ ifs.constants $ generateConstantFragments hsTyName . (.entry)
    genOperations = do
      V.forM_ ifs.operations \Attributed {entry = (Operation msp reg)} -> do
        do
          case msp of
            Just {} -> do
              -- FIXME: Implement special operations!
              pure ()
            Nothing | RegularOperation retTy (Just jsFunName) args <- reg -> do
              let hsFunName = "js_fun_" <> toHaskellIdentifier jsFunName
                  jsFun =
                    MethodOf
                      (tyConOrVar (toTypeName hsTyName))
                      (toHaskellIdentifier jsFunName)
                  returnType = ioTy `appTy` toHaskellType retTy
                  ffiDec = renderJSFFIImport $ toJSFFIImport JSFFIImportSeed {async = isAsyncJSType retTy, ..}
              Writer.tell
                MainModuleFragments
                  { mainExports = DL.singleton (varName, hsFunName)
                  , decs = DL.singleton ffiDec
                  }
            _ -> pure ()
    -- FIXME: Implement this
    genStringifiers = pure ()
    genAttributes = do
      let atts = ifs.attributes <> V.map (fmap (ReadWrite,)) ifs.inheritedAttributes
      V.forM_ atts \Attributed {entry = (rw, Attribute ty attName)} ->
        do
          let readerName = "js_get_" <> att
              att = toHaskellIdentifier attName
              readerSig =
                T.pack $
                  pprint $
                    mkNormalFunTy
                      (tyConOrVar (toTypeName hsTyName))
                      (ioTy `appTy` toHaskellType ty.entry)
              reader =
                [trimming|
                  foreign import javascript unsafe "$$1.${att}"
                    ${readerName} :: ${readerSig}
                |]
          Writer.tell
            MainModuleFragments
              { mainExports = DL.singleton (varName, readerName)
              , decs = DL.singleton $ Left reader
              }
          when (rw == ReadWrite) do
            let writerName = "js_set_" <> toHaskellIdentifier attName
                writerSig =
                  T.pack $
                    pprint $
                      foldr1
                        mkNormalFunTy
                        [tyConOrVar (toTypeName hsTyName), toHaskellType ty.entry, ioTy `appTy` unitT]
                writer =
                  "foreign import javascript unsafe \"$1." <> att <> " = $2\" " <> writerName <> " :: " <> writerSig
            Writer.tell
              MainModuleFragments
                { mainExports = DL.singleton (varName, writerName)
                , decs = DL.singleton $ Left writer
                }

    -- FIXME: Implement this
    genIterables = pure ()
    -- FIXME: Implement this
    genAsyncIterables = pure ()
    -- FIXME: Implement this
    genStaticAttributes = pure ()
    -- FIXME: Implement this
    genStaticOperations = pure ()

generateConstantFragments ::
  ( Reader CodeGenEnv :> es
  , Writer.Writer MainModuleFragments :> es
  ) =>
  Identifier ->
  Const ->
  Eff es ()
generateConstantFragments hsTyName (Const ty kname val) = do
  let funName = "js_const_" <> hsTyName <> "_" <> kname
      tyStr = T.pack $ pprint $ toHaskellType ty
      valStr = T.pack $ pprint $ toHaskellValue val
      sig = [trimming|${funName} :: ${tyStr}|]
      def = [trimming|${funName} = ${valStr}|]
  Writer.tell
    MainModuleFragments
      { mainExports = DL.singleton (varName, funName)
      , decs = DL.fromList $ map Left [sig, def]
      }

renderJSFFIImport :: JSFFIImport -> Either Text (HsDecl GhcPs)
renderJSFFIImport JSFFIImport {..} =
  let sig = T.pack $ pprint signature
      safety
        | async = "safe"
        | otherwise = "unsafe"
   in Left
        [trimming|
          foreign import javascript ${safety} "${jsCode}" ${hsFunName} :: ${sig}
        |]

data JSFun = Call !Text | MethodOf !(HsType GhcPs) !Text
  deriving (Generic)

data JSFFIImportSeed = JSFFIImportSeed
  { hsFunName :: !Text
  , jsFun :: !JSFun
  , args :: !ArgumentList
  , returnType :: !(HsType GhcPs)
  , async :: !Bool
  }
  deriving (Generic)

class ToHaskellValue a where
  toHaskellValue :: a -> HsExpr GhcPs

instance ToHaskellValue ConstValue where
  toHaskellValue = \case
    Bool p ->
      HsVar noExtField $ L noAnn $ Unqual $ mkOccName dataName $ fromString $ show p
    Decimal p ->
      case floatingOrInteger @Double p of
        Right i -> integerE i
        Left {} -> floatE p
    Infinity -> either error unLoc $ parseExpr "1.0 / 0.0"
    MinusInfinity -> either error unLoc $ parseExpr "-1.0 / 0.0"
    NaN -> either error unLoc $ parseExpr "0.0 / 0.0"
    Integer i -> integerE i

argumentHsTypes :: ArgumentList -> [HsType GhcPs]
argumentHsTypes ArgumentList {..} =
  V.toList $
    V.map (toHaskellType . (\(Argument a _) -> a) . (.entry)) requiredArgs
      <> V.map (toHaskellType . Nullable . (\(OptionalArgument a _ _) -> a.entry) . (.entry)) optionalArgs
      <> foldMap
        ( \(Ellipsis ty _) ->
            V.singleton $
              toHaskellType $
                DFrozenArray $
                  Attributed mempty ty
        )
        (Compose ellipsis)

toJSFFIImport :: JSFFIImportSeed -> JSFFIImport
toJSFFIImport JSFFIImportSeed {..} =
  let reqArgNum = V.length args.requiredArgs
      optArgNum = V.length args.optionalArgs
      ellipsNum = if isNothing args.ellipsis then 0 else 1
      argsHs0 = argumentHsTypes args
      signature = foldr mkNormalFunTy returnType argsHs
      jsArgs =
        T.intercalate "," $
          V.toList $
            V.generate
              (reqArgNum + optArgNum + ellipsNum)
              ( \i ->
                  if i < reqArgNum + optArgNum
                    then T.pack $ '$' : show (i + 1 + offset)
                    else T.pack $ "... $" ++ show (i + 1 + offset)
              )
      (argsHs, offset, jsCode) = case jsFun of
        Call m -> (argsHs0, 0, m <> "(" <> jsArgs <> ")")
        MethodOf ty mth -> (ty : argsHs0, 1, "$1." <> mth <> "(" <> jsArgs <> ")")
   in JSFFIImport {..}

data JSFFIImport = JSFFIImport
  { hsFunName :: !Text
  , jsCode :: !Text
  , signature :: !(HsType GhcPs)
  , async :: !Bool
  }
  deriving (Generic)

normaliseTypeName :: Text -> Identifier
normaliseTypeName =
  ix 0 %~ C.toUpper

toConstructorName :: Identifier -> ArgumentList -> T.Text
toConstructorName name args =
  "js_cons_"
    <> name
    <> "_"
    <> T.intercalate
      "_"
      ( V.toList $
          V.map
            ((.entry) >>> \(Argument a _) -> toHaskellIdentifier a)
            args.requiredArgs
            <> V.map
              ((.entry) >>> \(OptionalArgument a _ _) -> toHaskellIdentifier $ Nullable a.entry)
              args.optionalArgs
            <> foldMap
              ((.entry) >>> \(Ellipsis a _) -> V.singleton $ toHaskellIdentifier a)
              args.ellipsis
      )

class ToHaskellIdentifier a where
  toHaskellIdentifier :: a -> T.Text

instance ToHaskellIdentifier OperationName where
  toHaskellIdentifier = \case
    IncludesOperation -> "includes"
    OperationNamed s -> s

instance ToHaskellIdentifier AttributeName where
  toHaskellIdentifier = \case
    AttributeName s -> s
    AsyncAttribute -> "async"
    RequiredAttribute -> "required"

instance (ToHaskellIdentifier a) => ToHaskellIdentifier (WithNullarity a) where
  toHaskellIdentifier = \case
    Nullable a -> "nullable_" <> toHaskellIdentifier a
    Plain a -> toHaskellIdentifier a

instance ToHaskellIdentifier StringType where
  toHaskellIdentifier = \case
    DOMString -> "DOMString"
    ByteString -> "ByteString"
    USVString -> "USVString"

instance ToHaskellIdentifier PrimType where
  toHaskellIdentifier = \case
    Short {} -> "short"
    Long {} -> "long"
    LongLong {} -> "longlong"
    Float {} -> "float"
    Double {} -> "double"
    Boolean {} -> "boolean"
    Byte {} -> "byte"
    Octet {} -> "octet"
    Bigint {} -> "bigint"

instance ToHaskellIdentifier BufferType where
  toHaskellIdentifier = \case
    ArrayBuffer -> "ArrayBuffer"
    DataView -> "DataView"
    Int8Array -> "Int8Array"
    Uint8Array -> "Uint8Array"
    Uint8ClampedArray -> "Uint8ClampedArray"
    Int16Array -> "Int16Array"
    Uint16Array -> "Uint16Array"
    Int32Array -> "Int32Array"
    Uint32Array -> "Uint32Array"
    Float32Array -> "Float32Array"
    Float64Array -> "Float64Array"
    SharedArrayBuffer -> "SharedArrayBuffer"
    BigInt64Array -> "BigInt64Array"
    BigUint64Array -> "BigUint64Array"

instance ToHaskellIdentifier UnionType where
  toHaskellIdentifier (MkUnionType union) =
    T.intercalate
      "_"
      ( map (toHaskellIdentifier . fmap (Bi.first (.entry))) $
          NE.toList union
      )
      <> "_EndUnion"

instance (ToHaskellIdentifier a, ToHaskellIdentifier b) => ToHaskellIdentifier (Either a b) where
  toHaskellIdentifier = either toHaskellIdentifier toHaskellIdentifier

instance ToHaskellIdentifier DistinguishableType where
  toHaskellIdentifier = \case
    DPrim p -> toHaskellIdentifier p
    DString s -> toHaskellIdentifier s
    DNamed s -> s
    DSequence s -> "sequence_" <> toHaskellIdentifier s.entry
    DObject -> "object"
    DSymbol -> "symbol"
    DBuffer b -> toHaskellIdentifier b
    DFrozenArray f -> "FrozenArray_" <> toHaskellIdentifier f.entry
    DObservableArray o -> "ObservableArray_" <> toHaskellIdentifier o.entry
    DRecord str r -> "Record_" <> toHaskellIdentifier str <> "_" <> toHaskellIdentifier r.entry
    DUndefined -> "undefined"

instance ToHaskellIdentifier IDLType where
  toHaskellIdentifier = \case
    Distinguishable dt -> toHaskellIdentifier dt
    AnyType -> "any"
    PromiseType p -> "Promise_" <> toHaskellIdentifier p
    UnionType u -> "Union_" <> toHaskellIdentifier u

presetImports :: [Text]
presetImports = ["GHC.Wasm.Object.Builtins", "Foreign.C.Types", "Data.Int", "Data.Word"]

class ToHaskellType a where
  toHaskellType :: a -> HsType GhcPs
  isAsyncJSType :: a -> Bool
  toHaskellPrototype :: a -> HsType GhcPs

instance ToHaskellType ConstType where
  isAsyncJSType = \case
    PrimConstType p -> isAsyncJSType p
    IdentConstType _ -> False -- FIXME: should we lookup?
  toHaskellType = \case
    PrimConstType p -> toHaskellType p
    IdentConstType i -> tyConOrVar $ toTypeName i
  toHaskellPrototype = \case
    PrimConstType p -> toHaskellPrototype p
    IdentConstType i -> tyConOrVar $ toPrototypeName i

instance (ToHaskellType a) => ToHaskellType (WithNullarity a) where
  isAsyncJSType = \case
    Plain a -> isAsyncJSType a
    Nullable a -> isAsyncJSType a
  toHaskellType = \case
    Plain a -> toHaskellType a
    Nullable a -> tyConOrVar "Nullable" `appTy` toHaskellPrototype a
  toHaskellPrototype = \case
    Plain a -> toHaskellPrototype a
    Nullable a -> tyConOrVar "NullableClass" `appTy` toHaskellPrototype a

instance ToHaskellType StringType where
  isAsyncJSType = const False
  toHaskellType = \case
    DOMString -> tyConOrVar "DOMString"
    ByteString -> tyConOrVar "JSByteString"
    USVString -> tyConOrVar "USVString"
  toHaskellPrototype = \case
    DOMString -> tyConOrVar "DOMStringClass"
    ByteString -> tyConOrVar "JSByteStringClass"
    USVString -> tyConOrVar "USVStringClass"

instance ToHaskellType PrimType where
  isAsyncJSType = const False
  toHaskellType = \case
    Short Signed -> tyConOrVar "Int16"
    Short Unsigned -> tyConOrVar "Word16"
    Long Signed -> tyConOrVar "Int32"
    Long Unsigned -> tyConOrVar "Word32"
    LongLong Signed -> tyConOrVar "Int64"
    LongLong Unsigned -> tyConOrVar "Word64"
    -- FIXME: unrestrictedness?
    Float {} -> tyConOrVar "Float"
    -- FIXME: unrestrictedness?
    Double {} -> tyConOrVar "Double"
    Boolean {} -> tyConOrVar "Bool"
    Byte {} -> tyConOrVar "Int8"
    Octet {} -> tyConOrVar "Word8"
    Bigint {} -> tyConOrVar "BigInt"
  toHaskellPrototype =
    \case
      Short Signed -> primCls `appTy` tyConOrVar "Int16"
      Short Unsigned -> primCls `appTy` tyConOrVar "Word16"
      Long Signed -> primCls `appTy` tyConOrVar "Int32"
      Long Unsigned -> primCls `appTy` tyConOrVar "Word32"
      LongLong Signed -> primCls `appTy` tyConOrVar "Int64"
      LongLong Unsigned -> primCls `appTy` tyConOrVar "Word64"
      -- FIXME: unrestrictedness?
      Float {} -> primCls `appTy` tyConOrVar "Float"
      -- FIXME: unrestrictedness?
      Double {} -> primCls `appTy` tyConOrVar "Double"
      Boolean {} -> primCls `appTy` tyConOrVar "Bool"
      Byte {} -> primCls `appTy` tyConOrVar "Int8"
      Octet {} -> primCls `appTy` tyConOrVar "Word8"
      Bigint {} -> tyConOrVar "BigIntClass"

instance ToHaskellType BufferType where
  isAsyncJSType = const False
  toHaskellType = \case
    ArrayBuffer -> tyConOrVar "ArrayBuffer"
    DataView -> tyConOrVar "DataView"
    Int8Array -> tyConOrVar "JSByteArray" `appTy` tyConOrVar "Int8"
    Uint8Array -> tyConOrVar "JSByteArray" `appTy` tyConOrVar "Word8"
    -- FIXME: Is this correct?
    Uint8ClampedArray -> tyConOrVar "JSByteArray" `appTy` tyConOrVar "Word8"
    Int16Array -> tyConOrVar "JSByteArray" `appTy` tyConOrVar "Int16"
    Uint16Array -> tyConOrVar "JSByteArray" `appTy` tyConOrVar "Word16"
    Int32Array -> tyConOrVar "JSByteArray" `appTy` tyConOrVar "Int32"
    Uint32Array -> tyConOrVar "JSByteArray" `appTy` tyConOrVar "Word32"
    Float32Array -> tyConOrVar "JSByteArray" `appTy` tyConOrVar "Float"
    Float64Array -> tyConOrVar "JSByteArray" `appTy` tyConOrVar "Double"
    SharedArrayBuffer -> tyConOrVar "SharedArrayBuffer"
    BigInt64Array -> tyConOrVar "JSByteArray" `appTy` tyConOrVar "Int64"
    BigUint64Array -> tyConOrVar "JSByteArray" `appTy` tyConOrVar "Word64"
  toHaskellPrototype = \case
    ArrayBuffer -> tyConOrVar "ArrayBufferClass"
    DataView -> tyConOrVar "DataViewClass"
    Int8Array -> tyConOrVar "JSByteArrayClass" `appTy` tyConOrVar "Int8"
    Uint8Array -> tyConOrVar "JSByteArrayClass" `appTy` tyConOrVar "Word8"
    -- FIXME: Is this correct?
    Uint8ClampedArray -> tyConOrVar "JSByteArrayClass" `appTy` tyConOrVar "Word8"
    Int16Array -> tyConOrVar "JSByteArrayClass" `appTy` tyConOrVar "Int16"
    Uint16Array -> tyConOrVar "JSByteArrayClass" `appTy` tyConOrVar "Word16"
    Int32Array -> tyConOrVar "JSByteArrayClass" `appTy` tyConOrVar "Int32"
    Uint32Array -> tyConOrVar "JSByteArrayClass" `appTy` tyConOrVar "Word32"
    Float32Array -> tyConOrVar "JSByteArrayClass" `appTy` tyConOrVar "Float"
    Float64Array -> tyConOrVar "JSByteArrayClass" `appTy` tyConOrVar "Double"
    SharedArrayBuffer -> tyConOrVar "SharedArrayBufferClass"
    BigInt64Array -> tyConOrVar "JSByteArrayClass" `appTy` tyConOrVar "Int64"
    BigUint64Array -> tyConOrVar "JSByteArrayClass" `appTy` tyConOrVar "Word64"

instance ToHaskellType DistinguishableType where
  isAsyncJSType = \case
    DPrim p -> isAsyncJSType p
    DString s -> isAsyncJSType s
    DNamed _ -> False
    DSequence {} -> False
    DObject -> False
    DSymbol -> False
    DBuffer b -> isAsyncJSType b
    DFrozenArray {} -> False
    DObservableArray {} -> False
    DRecord {} -> False
    DUndefined -> False
  toHaskellPrototype = \case
    DPrim p -> toHaskellPrototype p
    DString s -> toHaskellPrototype s
    DNamed s -> tyConOrVar $ toPrototypeName s
    DSequence s -> tyConOrVar "SequenceClass" `appTy` toHaskellPrototype s.entry
    DObject -> tyConOrVar "AnyClass"
    DSymbol -> tyConOrVar "JSSymbolClass"
    DBuffer b -> toHaskellPrototype b
    DFrozenArray f -> tyConOrVar "FrozenArrayClass" `appTy` toHaskellPrototype f.entry
    DObservableArray o -> tyConOrVar "ObservableArrayClass" `appTy` toHaskellPrototype o.entry
    DRecord str r -> tyConOrVar "JSRecordClass" `appTy` toHaskellPrototype str `appTy` toHaskellPrototype r.entry
    DUndefined -> tyConOrVar "UndefinedClass"

  toHaskellType = \case
    DPrim p -> toHaskellType p
    DString s -> toHaskellType s
    DNamed s -> tyConOrVar $ toTypeName s
    DSequence s -> tyConOrVar "Sequence" `appTy` toHaskellPrototype s.entry
    DObject -> tyConOrVar "JSAny"
    DSymbol -> tyConOrVar "JSSymbol"
    DBuffer b -> toHaskellType b
    DFrozenArray f -> tyConOrVar "FrozenArray" `appTy` toHaskellPrototype f.entry
    DObservableArray o -> tyConOrVar "ObservableArray" `appTy` toHaskellPrototype o.entry
    -- FIXME: Implement below in ghc-wasm-jsobjects
    DRecord str r -> tyConOrVar "JSRecord" `appTy` toHaskellPrototype str `appTy` toHaskellPrototype r.entry
    DUndefined -> unitT

instance ToHaskellType UnionType where
  isAsyncJSType = \case
    MkUnionType comps ->
      all (all $ either (all isAsyncJSType) isAsyncJSType) $
        NE.toList comps
  toHaskellPrototype (MkUnionType comps) =
    let comps' =
          map
            ( fmap (either (toHaskellPrototype . (.entry)) toHaskellPrototype)
                >>> \case
                  Nullable p -> tyConOrVar "NullableClass" `appTy` p
                  Plain p -> p
            )
            $ NE.toList comps
     in tyConOrVar "UnionClass" `appTy` promotedListTy comps'
  toHaskellType = appTy (tyConOrVar "JSObject") . toHaskellPrototype

instance ToHaskellType IDLType where
  isAsyncJSType = \case
    Distinguishable dt -> isAsyncJSType dt
    AnyType -> False
    PromiseType {} -> True
    UnionType u -> isAsyncJSType u
  toHaskellPrototype = \case
    Distinguishable dt -> toHaskellPrototype dt
    AnyType -> tyConOrVar "AnyClass"
    PromiseType p -> tyConOrVar "PromiseClass" `appTy` toHaskellPrototype p
    UnionType u -> toHaskellPrototype u
  toHaskellType = \case
    Distinguishable dt -> toHaskellType dt
    AnyType -> tyConOrVar "JSAny"
    PromiseType p -> tyConOrVar "Promise" `appTy` toHaskellPrototype p
    UnionType u -> toHaskellType u

primCls :: HsType GhcPs
primCls = tyConOrVar "JSPrimClass"
