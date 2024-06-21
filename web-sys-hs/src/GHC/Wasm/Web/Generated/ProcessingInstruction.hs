{-# OPTIONS_GHC -Wno-all #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE TypeData #-}
module GHC.Wasm.Web.Generated.ProcessingInstruction (
        ProcessingInstruction, ProcessingInstructionClass, js_get_target,
        js_get_sheet
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.CharacterData.Core
import GHC.Wasm.Web.Generated.ProcessingInstruction.Core
import GHC.Wasm.Web.Generated.StyleSheet.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.target" js_get_target
  :: ProcessingInstruction -> (IO DOMString)
foreign import javascript unsafe "$1.sheet" js_get_sheet
  :: ProcessingInstruction -> (IO (Nullable StyleSheetClass))
