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
module GHC.Wasm.Web.Generated.PublicKeyCredentialUserEntity.Core (
        PublicKeyCredentialUserEntityFields,
        PublicKeyCredentialUserEntityClass, PublicKeyCredentialUserEntity,
        ReifiedPublicKeyCredentialUserEntity
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type PublicKeyCredentialUserEntityFields =
    '[ '("displayName", DOMStringClass),
       '("icon", NullableClass USVStringClass),
       '("id", BufferSourceClass),
       '("name", DOMStringClass)]
type PublicKeyCredentialUserEntityClass =
    JSDictionaryClass PublicKeyCredentialUserEntityFields
type PublicKeyCredentialUserEntity =
    JSObject PublicKeyCredentialUserEntityClass
type ReifiedPublicKeyCredentialUserEntity =
    ReifiedDictionary PublicKeyCredentialUserEntityFields
