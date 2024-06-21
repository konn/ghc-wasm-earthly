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
module GHC.Wasm.Web.Generated.PublicKeyCredentialRpEntity.Core (
        PublicKeyCredentialRpEntityFields,
        PublicKeyCredentialRpEntityClass, PublicKeyCredentialRpEntity,
        ReifiedPublicKeyCredentialRpEntity
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type PublicKeyCredentialRpEntityFields =
    '[ '("icon", NullableClass USVStringClass),
       '("id", NullableClass DOMStringClass),
       '("name", DOMStringClass)]
type PublicKeyCredentialRpEntityClass =
    JSDictionaryClass PublicKeyCredentialRpEntityFields
type PublicKeyCredentialRpEntity =
    JSObject PublicKeyCredentialRpEntityClass
type ReifiedPublicKeyCredentialRpEntity =
    ReifiedDictionary PublicKeyCredentialRpEntityFields
