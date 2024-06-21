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
module GHC.Wasm.Web.Generated.IDBObjectStoreParameters.Core (
        IDBObjectStoreParametersFields, IDBObjectStoreParametersClass,
        IDBObjectStoreParameters, ReifiedIDBObjectStoreParameters
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type IDBObjectStoreParametersFields =
    '[ '("autoIncrement", NullableClass (JSPrimClass Bool)),
       '("keyPath",
         NullableClass (NullableClass (UnionClass '[DOMStringClass,
                                                    SequenceClass DOMStringClass])))]
type IDBObjectStoreParametersClass =
    JSDictionaryClass IDBObjectStoreParametersFields
type IDBObjectStoreParameters =
    JSObject IDBObjectStoreParametersClass
type ReifiedIDBObjectStoreParameters =
    ReifiedDictionary IDBObjectStoreParametersFields
