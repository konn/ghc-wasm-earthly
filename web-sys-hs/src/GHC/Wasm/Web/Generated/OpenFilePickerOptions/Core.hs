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
module GHC.Wasm.Web.Generated.OpenFilePickerOptions.Core (
        OpenFilePickerOptionsFields, OpenFilePickerOptionsClass,
        OpenFilePickerOptions, ReifiedOpenFilePickerOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.FilePickerAcceptType.Core
import GHC.Wasm.Web.Generated.StartInDirectory.Core
import GHC.Wasm.Web.Types
type OpenFilePickerOptionsFields =
    '[ '("excludeAcceptAllOption", NullableClass (JSPrimClass Bool)),
       '("id", NullableClass DOMStringClass),
       '("multiple", NullableClass (JSPrimClass Bool)),
       '("startIn", NullableClass StartInDirectoryClass),
       '("types",
         NullableClass (SequenceClass FilePickerAcceptTypeClass))]
type OpenFilePickerOptionsClass =
    JSDictionaryClass OpenFilePickerOptionsFields
type OpenFilePickerOptions = JSObject OpenFilePickerOptionsClass
type ReifiedOpenFilePickerOptions =
    ReifiedDictionary OpenFilePickerOptionsFields
