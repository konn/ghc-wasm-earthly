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
module GHC.Wasm.Web.Generated.FilePickerAcceptType.Core (
        FilePickerAcceptTypeFields, FilePickerAcceptTypeClass,
        FilePickerAcceptType, ReifiedFilePickerAcceptType
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type FilePickerAcceptTypeFields =
    '[ '("accept",
         NullableClass (JSRecordClass USVStringClass (UnionClass '[USVStringClass,
                                                                   SequenceClass USVStringClass]))),
       '("description", NullableClass USVStringClass)]
type FilePickerAcceptTypeClass =
    JSDictionaryClass FilePickerAcceptTypeFields
type FilePickerAcceptType = JSObject FilePickerAcceptTypeClass
type ReifiedFilePickerAcceptType =
    ReifiedDictionary FilePickerAcceptTypeFields
