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
  generateWasmBinding,
  generateWasmBindingPure,
  generateWasmBindingWith,
  runFileTreePure,
  execFileTreePure,
) where

import Control.Arrow ((>>>))
import Control.Exception.Safe (throwIO, throwString)
import Control.Foldl qualified as L
import Control.Lens (imapM_, ix, (%~))
import Control.Monad (when, (<=<))
import Data.Bifunctor qualified as Bi
import Data.Char qualified as C
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Data (Data)
import Data.Foldable1 (intercalate1)
import Data.Functor.Compose (Compose (..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, fromMaybe, isNothing, maybeToList)
import Data.Monoid (First (getFirst))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing)
import Effectful.FileSystem.IO.ByteString qualified as Eff
import Effectful.FileSystem.IO.File (writeBinaryFile)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Reader.Static qualified as EffR
import Effectful.Writer.Static.Local (execWriter)
import Effectful.Writer.Static.Local qualified as Writer
import Effectful.Writer.Static.Shared qualified as SWriter
import GHC.Generics (Generic, Generically (..))
import GHC.Types.Name
import GHC.Types.Name.Reader (RdrName (..))
import GHC.Types.SrcLoc
import Language.Haskell.Parser.Ex.Helper
import Language.WebIDL.AST.Parser (parseIDLFragment)
import Language.WebIDL.AST.Types (Attribute (..), AttributeName (..), BufferType (..), DistinguishableType (..), IDLFragment, PrimType (..), Sign (..), StringType (..), UnionType (..), WithNullarity (..))
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
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

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

generateWasmBindingWith ::
  (FileSystem :> es) =>
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
  !definitions <- either throwIO pure $ desugar idls
  let !unknownTypes = fromMaybe mempty $ getUnknownIdentifiers definitions
  runFileTreeIOIn opts'.outputDir $
    runReader
      CodeGenEnv {modulePrefix = opts.modulePrefix, ..}
      generateWasmBinding

generateWasmBindingPure ::
  (Foldable t, FileSystem :> es) =>
  Maybe Text ->
  t IDLFragment ->
  Eff es (Map (Path Rel File) String)
generateWasmBindingPure modulePrefix idls = do
  !definitions <- either throwIO pure $ desugar idls
  let !unknownTypes = fromMaybe mempty $ getUnknownIdentifiers definitions
  execFileTreePure $
    runReader
      CodeGenEnv {modulePrefix = modulePrefix, ..}
      generateWasmBinding

generateWasmBinding ::
  (FileTree :> es, Reader CodeGenEnv :> es) =>
  Eff es ()
generateWasmBinding = do
  generateCoreModules
  generateMainModules

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
  , definitions :: !Definitions
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

generateCoreModules ::
  ( FileTree :> es
  , Reader CodeGenEnv :> es
  ) =>
  Eff es ()
generateCoreModules = do
  defns <- EffR.asks @CodeGenEnv (.definitions)
  imapM_ generateInterfaceCoreModule defns.interfaces

generateMainModules ::
  ( FileTree :> es
  , Reader CodeGenEnv :> es
  ) =>
  Eff es ()
generateMainModules = do
  defns <- EffR.asks @CodeGenEnv (.definitions)
  imapM_ generateInterfaceMainModule defns.interfaces

generateInterfaceCoreModule ::
  ( FileTree :> es
  , Reader CodeGenEnv :> es
  ) =>
  Text ->
  Attributed Interface ->
  Eff es ()
generateInterfaceCoreModule (normaliseTypeName -> name) Attributed {entry = ifs} = do
  moduleName <- toCoreModuleName name
  parentMod <- mapM toCoreModuleName $ getFirst ifs.parent
  let dest = fromJust (parseRelDir $ T.unpack name) </> [relfile|Core.hs|]
      imports = presetImports ++ maybeToList parentMod
      proto = toPrototypeName name
      protoDef =
        [trimming|type data ${proto} :: Prototype|]
      aliasDef = [trimming|type ${name} = JSObject ${proto}|]
      superDef = case getFirst ifs.parent of
        Just p ->
          let super = toPrototypeName p
           in [trimming|type instance SuperclassOf ${proto} = 'Just ${super}|]
        Nothing -> [trimming|type instance SuperclassOf ${proto} = 'Nothing|]
      decls = map Left [protoDef, aliasDef, superDef]
      exports = Just [(tvName, proto), (tvName, name)]

  either throwString (putFile dest) $ renderModule Module {..}

skipIfContainsUnknown :: (Data a, Reader CodeGenEnv :> es) => a -> Eff es () -> Eff es ()
skipIfContainsUnknown inp act = do
  unknowns <- EffR.asks @CodeGenEnv (.unknownTypes)
  when (Set.null $ unknowns `Set.intersection` L.foldOver namedTypeIdentifiers L.set inp) act

data MainModuleFragments = MainModuleFragments
  { mainExports :: DList (NameSpace, Text)
  , decs :: DList (Either Text (HsDecl GhcPs))
  }
  deriving (Generic)
  deriving (Semigroup, Monoid) via Generically MainModuleFragments

generateInterfaceMainModule ::
  forall es.
  ( FileTree :> es
  , Reader CodeGenEnv :> es
  ) =>
  Text ->
  Attributed Interface ->
  Eff es ()
generateInterfaceMainModule (normaliseTypeName -> name) Attributed {entry = ifs} = do
  moduleName <- toMainModuleName name
  let dest = fromJust (parseRelFile $ T.unpack name <> ".hs")
  unknowns <- EffR.asks @CodeGenEnv (.unknownTypes)
  let idents = Set.toList $ L.foldOver namedTypeIdentifiers L.set ifs Set.\\ unknowns
  parentMods <- mapM toCoreModuleName idents
  MainModuleFragments {..} <- execWriter go
  let imports = presetImports ++ parentMods
      proto = toPrototypeName name
      exports =
        Just $
          (tvName, name)
            : (tvName, proto)
            : DL.toList mainExports
      decls = DL.toList decs
  either throwString (putFile dest) $
    renderModule Module {..}
  where
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
    genConstructors = V.forM_ ifs.constructors \Attributed {entry = args} -> skipIfContainsUnknown args do
      let hsFunName
            | V.length ifs.constructors /= 1 = toConstructorName name args
            | otherwise = "js_cons_" <> name
          jsFun = Call name
          returnType = ioTy `appTy` tyConOrVar (toTypeName name)
          ffiDec = renderJSFFIImport $ toJSFFIImport JSFFIImportSeed {async = False, ..}
      Writer.tell
        MainModuleFragments
          { mainExports = DL.singleton (varName, hsFunName)
          , decs = DL.singleton ffiDec
          }

    -- FIXME: Implement this
    genConstants = pure ()
    genOperations = do
      V.forM_ ifs.operations \Attributed {entry = op@(Operation msp reg)} -> do
        skipIfContainsUnknown op do
          case msp of
            Just {} -> do
              -- FIXME: Implement special operations!
              pure ()
            Nothing | RegularOperation retTy (Just jsFunName) args <- reg -> do
              let hsFunName = "js_fun_" <> toHaskellIdentifier jsFunName
                  jsFun =
                    MethodOf
                      (tyConOrVar (toTypeName name))
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
      V.forM_ atts \Attributed {entry = (rw, attrib@(Attribute ty attName))} ->
        skipIfContainsUnknown attrib do
          let readerName = "js_get_" <> att
              att = toHaskellIdentifier attName
              readerSig =
                T.pack $
                  pprint $
                    mkNormalFunTy
                      (tyConOrVar (toTypeName name))
                      (ioTy `appTy` toHaskellType ty.entry)
              reader =
                "foreign import javascript unsafe \"$1." <> att <> "\" " <> readerName <> " :: " <> readerSig
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
                        [tyConOrVar (toTypeName name), toHaskellType ty.entry, ioTy `appTy` unitT]
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

toJSFFIImport :: JSFFIImportSeed -> JSFFIImport
toJSFFIImport JSFFIImportSeed {..} =
  let reqArgNum = V.length args.requiredArgs
      optArgNum = V.length args.optionalArgs
      ellipsNum = if isNothing args.ellipsis then 0 else 1
      argsHs0 =
        V.toList $
          V.map (toHaskellType . (\(Argument a _) -> a) . (.entry)) args.requiredArgs
            <> V.map (toHaskellType . Nullable . (\(OptionalArgument a _ _) -> a.entry) . (.entry)) args.optionalArgs
            <> foldMap
              ( \(Ellipsis ty _) ->
                  V.singleton $
                    toHaskellType $
                      DFrozenArray $
                        Attributed mempty ty
              )
              (Compose args.ellipsis)
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
    ByteString -> tyConOrVar "ByteStringClass"
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
    -- FIXME: implement below in ghc-wasm-jsobjects
    ArrayBuffer -> tyConOrVar "ArrayBuffer"
    DataView -> tyConOrVar "DataView"
    Int8Array -> tyConOrVar "Ptr" `appTy` tyConOrVar "Int8"
    Uint8Array -> tyConOrVar "Ptr" `appTy` tyConOrVar "Word8"
    -- FIXME: Is this correct?
    Uint8ClampedArray -> tyConOrVar "Ptr" `appTy` tyConOrVar "Word8"
    Int16Array -> tyConOrVar "Ptr" `appTy` tyConOrVar "Int16"
    Uint16Array -> tyConOrVar "Ptr" `appTy` tyConOrVar "Word16"
    Int32Array -> tyConOrVar "Ptr" `appTy` tyConOrVar "Int32"
    Uint32Array -> tyConOrVar "Ptr" `appTy` tyConOrVar "Word32"
    Float32Array -> tyConOrVar "Ptr" `appTy` tyConOrVar "Float"
    Float64Array -> tyConOrVar "Ptr" `appTy` tyConOrVar "Double"
    -- FIXME: implement below in ghc-wasm-jsobjects
    SharedArrayBuffer -> tyConOrVar "SharedArrayBuffer"
    BigInt64Array -> tyConOrVar "Ptr" `appTy` tyConOrVar "Int64"
    BigUint64Array -> tyConOrVar "Ptr" `appTy` tyConOrVar "Word64"
  toHaskellPrototype = \case
    -- FIXME: implement below in ghc-wasm-jsobjects
    ArrayBuffer -> tyConOrVar "ArrayBufferClass"
    -- FIXME: implement below in ghc-wasm-jsobjects
    DataView -> tyConOrVar "DataViewClass"
    Int8Array -> primCls `appTy` (tyConOrVar "Ptr" `appTy` tyConOrVar "Int8")
    Uint8Array -> primCls `appTy` (tyConOrVar "Ptr" `appTy` tyConOrVar "Word8")
    -- FIXME: Is this correct?
    Uint8ClampedArray -> primCls `appTy` (tyConOrVar "Ptr" `appTy` tyConOrVar "Word8")
    Int16Array -> primCls `appTy` (tyConOrVar "Ptr" `appTy` tyConOrVar "Int16")
    Uint16Array -> primCls `appTy` (tyConOrVar "Ptr" `appTy` tyConOrVar "Word16")
    Int32Array -> primCls `appTy` (tyConOrVar "Ptr" `appTy` tyConOrVar "Int32")
    Uint32Array -> primCls `appTy` (tyConOrVar "Ptr" `appTy` tyConOrVar "Word32")
    Float32Array -> primCls `appTy` (tyConOrVar "Ptr" `appTy` tyConOrVar "Float")
    Float64Array -> primCls `appTy` (tyConOrVar "Ptr" `appTy` tyConOrVar "Double")
    -- FIXME: implement below in ghc-wasm-jsobjects
    SharedArrayBuffer -> tyConOrVar "SharedArrayBufferClass"
    BigInt64Array -> primCls `appTy` (tyConOrVar "Ptr" `appTy` tyConOrVar "Int64")
    BigUint64Array -> primCls `appTy` (tyConOrVar "Ptr" `appTy` tyConOrVar "Word64")

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
    DSequence s -> tyConOrVar "JSSequenceClass" `appTy` toHaskellPrototype s.entry
    DObject -> tyConOrVar "AnyClass"
    -- FIXME: Implement below in ghc-wasm-jsobjects
    DSymbol -> tyConOrVar "JSSymbolClass"
    DBuffer b -> toHaskellPrototype b
    -- FIXME: Implement below in ghc-wasm-jsobjects
    DFrozenArray f -> tyConOrVar "FrozenArrayClass" `appTy` toHaskellPrototype f.entry
    -- FIXME: Implement below in ghc-wasm-jsobjects
    DObservableArray o -> tyConOrVar "ObservableArrayClass" `appTy` toHaskellPrototype o.entry
    -- FIXME: Implement below in ghc-wasm-jsobjects
    DRecord str r -> tyConOrVar "RecordClass" `appTy` toHaskellPrototype str `appTy` toHaskellType r.entry
    DUndefined -> tyConOrVar "UndefinedClass"

  toHaskellType = \case
    DPrim p -> toHaskellType p
    DString s -> toHaskellType s
    DNamed s -> tyConOrVar $ toTypeName s
    DSequence s -> tyConOrVar "JSSequence" `appTy` toHaskellPrototype s.entry
    DObject -> tyConOrVar "JSAny"
    -- FIXME: Implement below in ghc-wasm-jsobjects
    DSymbol -> tyConOrVar "JSSymbol"
    DBuffer b -> toHaskellType b
    -- FIXME: Implement below in ghc-wasm-jsobjects
    DFrozenArray f -> tyConOrVar "FrozenArray" `appTy` toHaskellPrototype f.entry
    -- FIXME: Implement below in ghc-wasm-jsobjects
    DObservableArray o -> tyConOrVar "ObservableArray" `appTy` toHaskellPrototype o.entry
    -- FIXME: Implement below in ghc-wasm-jsobjects
    DRecord str r -> tyConOrVar "Record" `appTy` toHaskellPrototype str `appTy` toHaskellPrototype r.entry
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
    PromiseType p -> tyConOrVar "PromiseClass" `appTy` toHaskellType p
    UnionType u -> toHaskellPrototype u
  toHaskellType = \case
    Distinguishable dt -> toHaskellType dt
    AnyType -> tyConOrVar "JSAny"
    PromiseType p -> tyConOrVar "Promise" `appTy` toHaskellType p
    UnionType u -> toHaskellType u

primCls :: HsType GhcPs
primCls = tyConOrVar "JSPrimClass"
