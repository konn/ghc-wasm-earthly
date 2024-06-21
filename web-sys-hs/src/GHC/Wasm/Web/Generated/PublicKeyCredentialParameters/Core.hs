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
module GHC.Wasm.Web.Generated.PublicKeyCredentialParameters.Core (
        PublicKeyCredentialParametersFields,
        PublicKeyCredentialParametersClass, PublicKeyCredentialParameters,
        ReifiedPublicKeyCredentialParameters
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.COSEAlgorithmIdentifier.Core
import GHC.Wasm.Web.Generated.PublicKeyCredentialType.Core
import GHC.Wasm.Web.Types
type PublicKeyCredentialParametersFields =
    '[ '("alg", COSEAlgorithmIdentifierClass),
       '("type", PublicKeyCredentialTypeClass)]
type PublicKeyCredentialParametersClass =
    JSDictionaryClass PublicKeyCredentialParametersFields
type PublicKeyCredentialParameters =
    JSObject PublicKeyCredentialParametersClass
type ReifiedPublicKeyCredentialParameters =
    ReifiedDictionary PublicKeyCredentialParametersFields
