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
module GHC.Wasm.Web.Generated.CredentialCreationOptions.Core (
        CredentialCreationOptionsFields, CredentialCreationOptionsClass,
        CredentialCreationOptions, ReifiedCredentialCreationOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AbortSignal.Core
import GHC.Wasm.Web.Generated.PublicKeyCredentialCreationOptions.Core
import GHC.Wasm.Web.Types
type CredentialCreationOptionsFields =
    '[ '("publicKey",
         NullableClass PublicKeyCredentialCreationOptionsClass),
       '("signal", NullableClass AbortSignalClass)]
type CredentialCreationOptionsClass =
    JSDictionaryClass CredentialCreationOptionsFields
type CredentialCreationOptions =
    JSObject CredentialCreationOptionsClass
type ReifiedCredentialCreationOptions =
    ReifiedDictionary CredentialCreationOptionsFields
