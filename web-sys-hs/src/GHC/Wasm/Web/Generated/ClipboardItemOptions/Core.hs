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
module GHC.Wasm.Web.Generated.ClipboardItemOptions.Core (
        ClipboardItemOptionsFields, ClipboardItemOptionsClass,
        ClipboardItemOptions, ReifiedClipboardItemOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.PresentationStyle.Core
import GHC.Wasm.Web.Types
type ClipboardItemOptionsFields =
    '[ '("presentationStyle", NullableClass PresentationStyleClass)]
type ClipboardItemOptionsClass =
    JSDictionaryClass ClipboardItemOptionsFields
type ClipboardItemOptions = JSObject ClipboardItemOptionsClass
type ReifiedClipboardItemOptions =
    ReifiedDictionary ClipboardItemOptionsFields
