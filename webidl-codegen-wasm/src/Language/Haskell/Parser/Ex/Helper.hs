{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.Parser.Ex.Helper (
  fakeFlags,
  pprint,
  fromPResult,
  parseModule,
  parseExpr,
  parseDec,
  parseType,
  module GHC.Hs,
  parseImport,
  defaultExtensions,
  formatModule,
) where

import Data.Function ((&))
import GHC.Driver.DynFlags
import GHC.Driver.Ppr (showSDoc)
import GHC.Hs
import GHC.LanguageExtensions (Extension (..))
import GHC.Parser.Lexer (PState (..), ParseResult (..))
import GHC.Real ()
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import qualified Language.Haskell.GhclibParserEx.GHC.Parser as P
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)

fakeFlags :: DynFlags
fakeFlags =
  defaultDynFlags fakeSettings
    & flip (foldl xopt_set) defaultExtensions
    & flip lang_set (Just GHC2021)

defaultExtensions :: [Extension]
defaultExtensions =
  [ DataKinds
  , TypeFamilies
  , PolyKinds
  , TypeOperators
  , GADTs
  , MultiParamTypeClasses
  , OverloadedStrings
  , LambdaCase
  , RecordWildCards
  , OverloadedRecordDot
  , StandaloneKindSignatures
  , UnliftedDatatypes
  , UnliftedNewtypes
  , TypeData
  ]

pprint :: (Outputable a) => a -> String
pprint = showSDoc fakeFlags . ppr

fromPResult :: ParseResult a -> Either String a
fromPResult resl = case resl of
  POk _ a -> Right a
  PFailed st -> Left $ showSDoc fakeFlags $ ppr $ errors st

parseModule :: String -> Either String (Located (HsModule GhcPs))
parseModule src =
  fromPResult $ P.parseModule src fakeFlags

parseExpr :: String -> Either String (GenLocated SrcSpanAnnA (HsExpr GhcPs))
parseExpr src =
  fromPResult $ P.parseExpression src fakeFlags

parseDec :: String -> Either String (GenLocated SrcSpanAnnA (HsDecl GhcPs))
parseDec src =
  fromPResult $ P.parseDeclaration src fakeFlags

parseType :: String -> Either String (GenLocated SrcSpanAnnA (HsType GhcPs))
parseType src =
  fromPResult $ P.parseType src fakeFlags

parseImport :: String -> Either String (GenLocated SrcSpanAnnA (ImportDecl GhcPs))
parseImport src =
  fromPResult $ P.parseImport src fakeFlags

formatModule :: HsModule GhcPs -> String
formatModule m =
  unlines $
    "{-# LANGUAGE GHC2021 #-}"
      : map (("{-# LANGUAGE " ++) . (++ " #-}") . pprint) defaultExtensions
      ++ [pprint m]
