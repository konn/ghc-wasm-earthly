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
  tyConOrVar,
  mkNormalFunTy,
  appTy,
  promotedListTy,
  ioTy,
  unitT,
  symbolLitTy,
  promotedTupleT,
  floatE,
  integerE,
) where

import Data.Function ((&))
import Data.Ratio ((%))
import Data.Scientific (Scientific)
import qualified Data.Scientific as S
import Data.String (IsString (..))
import qualified Data.Text as T
import GHC.Core.TyCo.Ppr (appPrec, funPrec)
import GHC.Driver.DynFlags
import GHC.Driver.Ppr (showSDoc)
import GHC.Hs
import GHC.LanguageExtensions (Extension (..))
import GHC.Parser.Lexer (PState (..), ParseResult (..))
import GHC.Real ()
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader (RdrName (..))
import GHC.Types.SourceText (FractionalExponentBase (Base10), FractionalLit (..), IntegralLit (..), SourceText (NoSourceText))
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
    "{-# OPTIONS_GHC -Wno-all #-}"
      : "{-# LANGUAGE GHC2021 #-}"
      : map (("{-# LANGUAGE " ++) . (++ " #-}") . pprint) defaultExtensions
      ++ [pprint m]

tyConOrVar :: T.Text -> HsType GhcPs
tyConOrVar =
  HsTyVar [] NotPromoted
    . L noAnn
    . Unqual
    . mkTyVarOcc
    . T.unpack

mkNormalFunTy :: HsType GhcPs -> HsType GhcPs -> HsType GhcPs
mkNormalFunTy l r =
  HsFunTy
    noExtField
    (HsUnrestrictedArrow NoEpUniTok)
    (parenthesizeHsType funPrec (L noAnn l))
    (parenthesizeHsType appPrec (L noAnn r))

appTy :: HsType GhcPs -> HsType GhcPs -> HsType GhcPs
appTy l r = HsAppTy noExtField (L noAnn l) (parenthesizeHsType appPrec $ L noAnn r)

promotedListTy :: [HsType GhcPs] -> HsType GhcPs
promotedListTy = HsExplicitListTy [] IsPromoted . map (L noAnn)

symbolLitTy :: T.Text -> HsType GhcPs
symbolLitTy = HsTyLit noExtField . HsStrTy NoSourceText . fromString . T.unpack

promotedTupleT :: HsType GhcPs -> HsType GhcPs -> HsType GhcPs
promotedTupleT l r = HsExplicitTupleTy [] [L noAnn l, L noAnn r]

ioTy :: HsType GhcPs
ioTy = tyConOrVar $ T.pack "IO"

unitT :: HsType GhcPs
unitT = HsTupleTy (AnnParen AnnParens noAnn noAnn) HsBoxedOrConstraintTuple []

floatE :: Scientific -> HsExpr GhcPs
floatE d =
  HsOverLit noExtField $
    mkHsFractional
      FL
        { fl_text = NoSourceText
        , fl_signi = S.coefficient d % 1
        , fl_neg = d < 0
        , fl_exp_base = Base10
        , fl_exp = fromIntegral $ S.base10Exponent d
        }

integerE :: Integer -> HsExpr GhcPs
integerE i =
  HsOverLit noExtField $
    mkHsIntegral $
      IL {il_value = i, il_text = NoSourceText, il_neg = False}