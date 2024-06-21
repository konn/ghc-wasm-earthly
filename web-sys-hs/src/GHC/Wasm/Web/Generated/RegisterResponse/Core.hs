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
module GHC.Wasm.Web.Generated.RegisterResponse.Core (
        RegisterResponseFields, RegisterResponseClass, RegisterResponse,
        ReifiedRegisterResponse
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ErrorCode.Core
import GHC.Wasm.Web.Types
type RegisterResponseFields =
    '[ '("clientData", NullableClass DOMStringClass),
       '("errorCode", NullableClass (NullableClass ErrorCodeClass)),
       '("errorMessage", NullableClass (NullableClass DOMStringClass)),
       '("registrationData", NullableClass DOMStringClass),
       '("version", NullableClass DOMStringClass)]
type RegisterResponseClass =
    JSDictionaryClass RegisterResponseFields
type RegisterResponse = JSObject RegisterResponseClass
type ReifiedRegisterResponse =
    ReifiedDictionary RegisterResponseFields
