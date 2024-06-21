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
module GHC.Wasm.Web.Generated.SaveFilePickerOptions.Core (
        SaveFilePickerOptionsFields, SaveFilePickerOptionsClass,
        SaveFilePickerOptions, ReifiedSaveFilePickerOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.FilePickerAcceptType.Core
import GHC.Wasm.Web.Generated.StartInDirectory.Core
import GHC.Wasm.Web.Types
type SaveFilePickerOptionsFields =
    '[ '("excludeAcceptAllOption", NullableClass (JSPrimClass Bool)),
       '("id", NullableClass DOMStringClass),
       '("startIn", NullableClass StartInDirectoryClass),
       '("suggestedName", NullableClass (NullableClass USVStringClass)),
       '("types",
         NullableClass (SequenceClass FilePickerAcceptTypeClass))]
type SaveFilePickerOptionsClass =
    JSDictionaryClass SaveFilePickerOptionsFields
type SaveFilePickerOptions = JSObject SaveFilePickerOptionsClass
type ReifiedSaveFilePickerOptions =
    ReifiedDictionary SaveFilePickerOptionsFields
