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
module GHC.Wasm.Web.Generated.CredentialsContainer (
        CredentialsContainer, CredentialsContainerClass,
        js_fun_get_nullable_CredentialRequestOptions_Promise_nullable_Credential,
        js_fun_create_nullable_CredentialCreationOptions_Promise_nullable_Credential,
        js_fun_store_Credential_Promise_Credential,
        js_fun_preventSilentAccess__Promise_undefined
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Credential.Core
import GHC.Wasm.Web.Generated.CredentialCreationOptions.Core
import GHC.Wasm.Web.Generated.CredentialRequestOptions.Core
import GHC.Wasm.Web.Generated.CredentialsContainer.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.get($2)" js_fun_get_nullable_CredentialRequestOptions_Promise_nullable_Credential
  :: CredentialsContainer
     -> (Nullable CredentialRequestOptionsClass
         -> (IO (Promise (NullableClass CredentialClass))))
foreign import javascript safe "$1.create($2)" js_fun_create_nullable_CredentialCreationOptions_Promise_nullable_Credential
  :: CredentialsContainer
     -> (Nullable CredentialCreationOptionsClass
         -> (IO (Promise (NullableClass CredentialClass))))
foreign import javascript safe "$1.store($2)" js_fun_store_Credential_Promise_Credential
  :: CredentialsContainer
     -> (Credential -> (IO (Promise CredentialClass)))
foreign import javascript safe "$1.preventSilentAccess()" js_fun_preventSilentAccess__Promise_undefined
  :: CredentialsContainer -> (IO (Promise UndefinedClass))
