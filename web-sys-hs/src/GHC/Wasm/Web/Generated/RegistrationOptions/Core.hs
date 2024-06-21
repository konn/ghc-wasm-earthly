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
module GHC.Wasm.Web.Generated.RegistrationOptions.Core (
        RegistrationOptionsFields, RegistrationOptionsClass,
        RegistrationOptions, ReifiedRegistrationOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ServiceWorkerUpdateViaCache.Core
import GHC.Wasm.Web.Types
type RegistrationOptionsFields =
    '[ '("scope", NullableClass USVStringClass),
       '("type", NullableClass USVStringClass),
       '("updateViaCache",
         NullableClass ServiceWorkerUpdateViaCacheClass)]
type RegistrationOptionsClass =
    JSDictionaryClass RegistrationOptionsFields
type RegistrationOptions = JSObject RegistrationOptionsClass
type ReifiedRegistrationOptions =
    ReifiedDictionary RegistrationOptionsFields
