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
module GHC.Wasm.Web.Generated.SignResponse.Core (
        SignResponseFields, SignResponseClass, SignResponse,
        ReifiedSignResponse
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ErrorCode.Core
import GHC.Wasm.Web.Types
type SignResponseFields =
    '[ '("clientData", NullableClass DOMStringClass),
       '("errorCode", NullableClass (NullableClass ErrorCodeClass)),
       '("errorMessage", NullableClass (NullableClass DOMStringClass)),
       '("keyHandle", NullableClass DOMStringClass),
       '("signatureData", NullableClass DOMStringClass)]
type SignResponseClass = JSDictionaryClass SignResponseFields
type SignResponse = JSObject SignResponseClass
type ReifiedSignResponse = ReifiedDictionary SignResponseFields
