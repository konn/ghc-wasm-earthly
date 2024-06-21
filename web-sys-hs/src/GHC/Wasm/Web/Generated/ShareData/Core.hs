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
module GHC.Wasm.Web.Generated.ShareData.Core (
        ShareDataFields, ShareDataClass, ShareData, ReifiedShareData
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.File.Core
import GHC.Wasm.Web.Types
type ShareDataFields =
    '[ '("files", NullableClass (SequenceClass FileClass)),
       '("text", NullableClass USVStringClass),
       '("title", NullableClass USVStringClass),
       '("url", NullableClass USVStringClass)]
type ShareDataClass = JSDictionaryClass ShareDataFields
type ShareData = JSObject ShareDataClass
type ReifiedShareData = ReifiedDictionary ShareDataFields
