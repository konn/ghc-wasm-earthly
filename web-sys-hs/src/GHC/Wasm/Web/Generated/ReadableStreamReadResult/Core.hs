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
module GHC.Wasm.Web.Generated.ReadableStreamReadResult.Core (
        ReadableStreamReadResultFields, ReadableStreamReadResultClass,
        ReadableStreamReadResult, ReifiedReadableStreamReadResult
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type ReadableStreamReadResultFields =
    '[ '("done", NullableClass (JSPrimClass Bool)),
       '("value", NullableClass AnyClass)]
type ReadableStreamReadResultClass =
    JSDictionaryClass ReadableStreamReadResultFields
type ReadableStreamReadResult =
    JSObject ReadableStreamReadResultClass
type ReifiedReadableStreamReadResult =
    ReifiedDictionary ReadableStreamReadResultFields
