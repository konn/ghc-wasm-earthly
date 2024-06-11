module Language.WebIDL.CodeGen.GHC.Wasm where

import GHC.Driver.DynFlags
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)

fakeFlags :: DynFlags
fakeFlags = defaultDynFlags fakeSettings
