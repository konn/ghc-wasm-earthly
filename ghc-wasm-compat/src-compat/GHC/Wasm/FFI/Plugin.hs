{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module GHC.Wasm.FFI.Plugin (plugin) where

import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Hs
import GHC.Parser
import GHC.Parser.Lexer (P, ParseResult (..), initParserState, unP)
import GHC.Parser.PostProcess (ECP (..), PV, runPV)
import GHC.Plugins hiding ((<>))
import qualified GHC.Types.Fixity as LF
import GHC.Types.ForeignCall (CCallConv (JavaScriptCallConv), CExportSpec (..))

plugin :: Plugin
plugin =
  defaultPlugin
    { pluginRecompile = purePlugin
    , parsedResultAction = const rewriteJSFFI
    }

rewriteJSFFI :: ModSummary -> ParsedResult -> Hsc ParsedResult
rewriteJSFFI _ms pr = do
  flags <- getDynFlags
  pure $
    pr
      { parsedResultModule =
          pr.parsedResultModule
            { hpm_module =
                rewriteModule flags <$> pr.parsedResultModule.hpm_module
            }
      }

rewriteModule :: DynFlags -> HsModule GhcPs -> HsModule GhcPs
rewriteModule flags hsm@HsModule {..} = hsm {hsmodDecls = concatMap go hsmodDecls}
  where
    go :: LHsDecl GhcPs -> [LHsDecl GhcPs]
    go (L _ (ForD _ (ForeignExport _ _ _ (CExport _ (L _ (CExportStatic _ _ JavaScriptCallConv)))))) = []
    go (L loc (ForD _ ci@(ForeignImport _ funName ty (CImport _ (L _ JavaScriptCallConv) _ _ _)))) = generateDummy flags loc ci funName ty
    go x = [x]

parseWith :: P a -> DynFlags -> String -> ParseResult a
parseWith p flags inp =
  unP p $ initParserState (initParserOpts flags) (stringToStringBuffer inp) $ mkRealSrcLoc (mkFastString "<string>") 1 1

parseExp :: DynFlags -> String -> LHsExpr GhcPs
parseExp flags s =
  case parseWith parseExpression flags s of
    POk state e ->
      let e' = e :: ECP
          parser_validator = unECP e' :: PV (LHsExpr GhcPs)
          parser = runPV parser_validator :: P (LHsExpr GhcPs)
       in case unP parser state :: ParseResult (LHsExpr GhcPs) of
            POk _ e'' -> e''
            PFailed _ -> error $ "parse failed: " <> s
    PFailed _ -> error $ "parse failed: " <> s

generateDummy :: DynFlags -> SrcSpanAnnA -> ForeignDecl GhcPs -> GenLocated SrcSpanAnnN RdrName -> GenLocated SrcSpanAnnA (HsSigType GhcPs) -> [GenLocated SrcSpanAnnA (HsDecl GhcPs)]
generateDummy flags loc ffi funName funType =
  let errorBody = parseExp flags $ "error " <> show (showSDoc flags (ppr ffi))
   in map
        (L loc)
        [ SigD NoExtField $ TypeSig noAnn [funName] $ HsWC NoExtField funType
        , ValD NoExtField $
            FunBind NoExtField funName $
              MG
                (Generated OtherExpansion DoPmc)
                ( L
                    noAnn
                    [ noSSA
                        $ Match
                          noAnn
                          ( FunRhs
                              { mc_strictness = NoSrcStrict
                              , mc_fun = funName
                              , mc_fixity = LF.Prefix
                              }
                          )
                          []
                        $ GRHSs
                          { grhssLocalBinds = HsValBinds noAnn $ ValBinds NoAnnSortKey mempty []
                          , grhssGRHSs =
                              [noLocA $ GRHS noAnn [] $ errorBody]
                          , grhssExt = EpaComments []
                          }
                    ]
                )
        ]
  where
    noSSA = L noSrcSpanA
