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
module GHC.Wasm.Web.Generated.PublicKeyCredentialDescriptor.Core (
        PublicKeyCredentialDescriptorFields,
        PublicKeyCredentialDescriptorClass, PublicKeyCredentialDescriptor,
        ReifiedPublicKeyCredentialDescriptor
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AuthenticatorTransport.Core
import GHC.Wasm.Web.Generated.PublicKeyCredentialType.Core
import GHC.Wasm.Web.Types
type PublicKeyCredentialDescriptorFields =
    '[ '("id", BufferSourceClass),
       '("transports",
         NullableClass (SequenceClass AuthenticatorTransportClass)),
       '("type", PublicKeyCredentialTypeClass)]
type PublicKeyCredentialDescriptorClass =
    JSDictionaryClass PublicKeyCredentialDescriptorFields
type PublicKeyCredentialDescriptor =
    JSObject PublicKeyCredentialDescriptorClass
type ReifiedPublicKeyCredentialDescriptor =
    ReifiedDictionary PublicKeyCredentialDescriptorFields
