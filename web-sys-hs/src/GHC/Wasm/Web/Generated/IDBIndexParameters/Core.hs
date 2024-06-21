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
module GHC.Wasm.Web.Generated.IDBIndexParameters.Core (
        IDBIndexParametersFields, IDBIndexParametersClass,
        IDBIndexParameters, ReifiedIDBIndexParameters
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type IDBIndexParametersFields =
    '[ '("locale", NullableClass (NullableClass DOMStringClass)),
       '("multiEntry", NullableClass (JSPrimClass Bool)),
       '("unique", NullableClass (JSPrimClass Bool))]
type IDBIndexParametersClass =
    JSDictionaryClass IDBIndexParametersFields
type IDBIndexParameters = JSObject IDBIndexParametersClass
type ReifiedIDBIndexParameters =
    ReifiedDictionary IDBIndexParametersFields
