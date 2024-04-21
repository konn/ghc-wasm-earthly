{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Wasm.FFI.Plugin (plugin) where

import GHC.Hs
import GHC.Plugins
import qualified GHC.Types.Fixity as LF
import GHC.Types.ForeignCall (CCallConv (JavaScriptCallConv), CExportSpec (..))
import GHC.Types.SourceText (SourceText (NoSourceText))

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
    go (L loc (ForD _ (ForeignImport _ funName ty ci@(CImport _ (L _ JavaScriptCallConv) _ _ _)))) = generateDummy flags loc ci funName ty
    go x = [x]

generateDummy :: DynFlags -> SrcSpanAnn' (EpAnn AnnListItem) -> ForeignImport GhcPs -> GenLocated SrcSpanAnnN RdrName -> GenLocated SrcSpanAnnA (HsSigType GhcPs) -> [GenLocated SrcSpanAnnA (HsDecl GhcPs)]
generateDummy flags loc ffi funName funType =
  map
    (L loc)
    [ SigD NoExtField $ TypeSig EpAnnNotUsed [funName] $ HsWC NoExtField funType
    , ValD NoExtField $
        FunBind NoExtField funName $
          MG
            (Generated DoPmc)
            ( noSSA
                [ noSSA
                    $ Match
                      EpAnnNotUsed
                      ( FunRhs
                          { mc_strictness = NoSrcStrict
                          , mc_fun = funName
                          , mc_fixity = LF.Prefix
                          }
                      )
                      []
                    $ GRHSs
                      { grhssLocalBinds = HsValBinds EpAnnNotUsed $ ValBinds NoAnnSortKey mempty []
                      , grhssGRHSs =
                          [ noSSA $
                              GRHS EpAnnNotUsed [] $
                                noSSA $
                                  HsApp
                                    EpAnnNotUsed
                                    ( noSSA $
                                        HsVar NoExtField $
                                          noSSA $
                                            Unqual $
                                              mkVarOcc "error"
                                    )
                                    ( noSSA $
                                        HsLit EpAnnNotUsed $
                                          HsString NoSourceText $
                                            fsLit $
                                              showSDoc flags $
                                                ppr ffi
                                    )
                          ]
                      , grhssExt = EpaComments []
                      }
                ]
            )
    ]
  where
    noSSA = L noSrcSpanA
