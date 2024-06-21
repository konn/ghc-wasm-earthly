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
module GHC.Wasm.Web.Generated.CredentialRequestOptions.Core (
        CredentialRequestOptionsFields, CredentialRequestOptionsClass,
        CredentialRequestOptions, ReifiedCredentialRequestOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AbortSignal.Core
import GHC.Wasm.Web.Generated.PublicKeyCredentialRequestOptions.Core
import GHC.Wasm.Web.Types
type CredentialRequestOptionsFields =
    '[ '("publicKey",
         NullableClass PublicKeyCredentialRequestOptionsClass),
       '("signal", NullableClass AbortSignalClass)]
type CredentialRequestOptionsClass =
    JSDictionaryClass CredentialRequestOptionsFields
type CredentialRequestOptions =
    JSObject CredentialRequestOptionsClass
type ReifiedCredentialRequestOptions =
    ReifiedDictionary CredentialRequestOptionsFields
