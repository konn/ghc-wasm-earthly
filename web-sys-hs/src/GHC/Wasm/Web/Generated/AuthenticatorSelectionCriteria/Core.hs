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
module GHC.Wasm.Web.Generated.AuthenticatorSelectionCriteria.Core (
        AuthenticatorSelectionCriteriaFields,
        AuthenticatorSelectionCriteriaClass,
        AuthenticatorSelectionCriteria,
        ReifiedAuthenticatorSelectionCriteria
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AuthenticatorAttachment.Core
import GHC.Wasm.Web.Generated.UserVerificationRequirement.Core
import GHC.Wasm.Web.Types
type AuthenticatorSelectionCriteriaFields =
    '[ '("authenticatorAttachment",
         NullableClass AuthenticatorAttachmentClass),
       '("requireResidentKey", NullableClass (JSPrimClass Bool)),
       '("userVerification",
         NullableClass UserVerificationRequirementClass)]
type AuthenticatorSelectionCriteriaClass =
    JSDictionaryClass AuthenticatorSelectionCriteriaFields
type AuthenticatorSelectionCriteria =
    JSObject AuthenticatorSelectionCriteriaClass
type ReifiedAuthenticatorSelectionCriteria =
    ReifiedDictionary AuthenticatorSelectionCriteriaFields
