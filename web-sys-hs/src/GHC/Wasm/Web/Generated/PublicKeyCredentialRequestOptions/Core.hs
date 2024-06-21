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
module GHC.Wasm.Web.Generated.PublicKeyCredentialRequestOptions.Core (
        PublicKeyCredentialRequestOptionsFields,
        PublicKeyCredentialRequestOptionsClass,
        PublicKeyCredentialRequestOptions,
        ReifiedPublicKeyCredentialRequestOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AuthenticationExtensionsClientInputs.Core
import GHC.Wasm.Web.Generated.PublicKeyCredentialDescriptor.Core
import GHC.Wasm.Web.Generated.UserVerificationRequirement.Core
import GHC.Wasm.Web.Types
type PublicKeyCredentialRequestOptionsFields =
    '[ '("allowCredentials",
         NullableClass (SequenceClass PublicKeyCredentialDescriptorClass)),
       '("challenge", BufferSourceClass),
       '("extensions",
         NullableClass AuthenticationExtensionsClientInputsClass),
       '("rpId", NullableClass USVStringClass),
       '("timeout", NullableClass (JSPrimClass Word32)),
       '("userVerification",
         NullableClass UserVerificationRequirementClass)]
type PublicKeyCredentialRequestOptionsClass =
    JSDictionaryClass PublicKeyCredentialRequestOptionsFields
type PublicKeyCredentialRequestOptions =
    JSObject PublicKeyCredentialRequestOptionsClass
type ReifiedPublicKeyCredentialRequestOptions =
    ReifiedDictionary PublicKeyCredentialRequestOptionsFields
