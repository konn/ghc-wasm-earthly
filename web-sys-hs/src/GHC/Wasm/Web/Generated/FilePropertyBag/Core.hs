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
module GHC.Wasm.Web.Generated.FilePropertyBag.Core (
        FilePropertyBagFields, FilePropertyBagClass, FilePropertyBag,
        ReifiedFilePropertyBag
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type FilePropertyBagFields =
    '[ '("lastModified", NullableClass (JSPrimClass Int64)),
       '("type", NullableClass DOMStringClass)]
type FilePropertyBagClass = JSDictionaryClass FilePropertyBagFields
type FilePropertyBag = JSObject FilePropertyBagClass
type ReifiedFilePropertyBag =
    ReifiedDictionary FilePropertyBagFields
