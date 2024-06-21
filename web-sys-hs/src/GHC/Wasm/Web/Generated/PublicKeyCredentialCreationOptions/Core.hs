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
module GHC.Wasm.Web.Generated.PublicKeyCredentialCreationOptions.Core (
        PublicKeyCredentialCreationOptionsFields,
        PublicKeyCredentialCreationOptionsClass,
        PublicKeyCredentialCreationOptions,
        ReifiedPublicKeyCredentialCreationOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AttestationConveyancePreference.Core
import GHC.Wasm.Web.Generated.AuthenticationExtensionsClientInputs.Core
import GHC.Wasm.Web.Generated.AuthenticatorSelectionCriteria.Core
import GHC.Wasm.Web.Generated.PublicKeyCredentialDescriptor.Core
import GHC.Wasm.Web.Generated.PublicKeyCredentialParameters.Core
import GHC.Wasm.Web.Generated.PublicKeyCredentialRpEntity.Core
import GHC.Wasm.Web.Generated.PublicKeyCredentialUserEntity.Core
import GHC.Wasm.Web.Types
type PublicKeyCredentialCreationOptionsFields =
    '[ '("attestation",
         NullableClass AttestationConveyancePreferenceClass),
       '("authenticatorSelection",
         NullableClass AuthenticatorSelectionCriteriaClass),
       '("challenge", BufferSourceClass),
       '("excludeCredentials",
         NullableClass (SequenceClass PublicKeyCredentialDescriptorClass)),
       '("extensions",
         NullableClass AuthenticationExtensionsClientInputsClass),
       '("pubKeyCredParams",
         SequenceClass PublicKeyCredentialParametersClass),
       '("rp", PublicKeyCredentialRpEntityClass),
       '("timeout", NullableClass (JSPrimClass Word32)),
       '("user", PublicKeyCredentialUserEntityClass)]
type PublicKeyCredentialCreationOptionsClass =
    JSDictionaryClass PublicKeyCredentialCreationOptionsFields
type PublicKeyCredentialCreationOptions =
    JSObject PublicKeyCredentialCreationOptionsClass
type ReifiedPublicKeyCredentialCreationOptions =
    ReifiedDictionary PublicKeyCredentialCreationOptionsFields
