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
module GHC.Wasm.Web.Generated.ResponseInit.Core (
        ResponseInitFields, ResponseInitClass, ResponseInit,
        ReifiedResponseInit
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.HeadersInit.Core
import GHC.Wasm.Web.Types
type ResponseInitFields =
    '[ '("headers", NullableClass HeadersInitClass),
       '("status", NullableClass (JSPrimClass Word16)),
       '("statusText", NullableClass JSByteStringClass)]
type ResponseInitClass = JSDictionaryClass ResponseInitFields
type ResponseInit = JSObject ResponseInitClass
type ReifiedResponseInit = ReifiedDictionary ResponseInitFields
