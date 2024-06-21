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
module GHC.Wasm.Web.Generated.BlobPropertyBag.Core (
        BlobPropertyBagFields, BlobPropertyBagClass, BlobPropertyBag,
        ReifiedBlobPropertyBag
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EndingTypes.Core
import GHC.Wasm.Web.Types
type BlobPropertyBagFields =
    '[ '("endings", NullableClass EndingTypesClass),
       '("type", NullableClass DOMStringClass)]
type BlobPropertyBagClass = JSDictionaryClass BlobPropertyBagFields
type BlobPropertyBag = JSObject BlobPropertyBagClass
type ReifiedBlobPropertyBag =
    ReifiedDictionary BlobPropertyBagFields
