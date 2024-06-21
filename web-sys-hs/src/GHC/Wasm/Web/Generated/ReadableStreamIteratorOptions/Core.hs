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
module GHC.Wasm.Web.Generated.ReadableStreamIteratorOptions.Core (
        ReadableStreamIteratorOptionsFields,
        ReadableStreamIteratorOptionsClass, ReadableStreamIteratorOptions,
        ReifiedReadableStreamIteratorOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type ReadableStreamIteratorOptionsFields =
    '[ '("preventCancel", NullableClass (JSPrimClass Bool))]
type ReadableStreamIteratorOptionsClass =
    JSDictionaryClass ReadableStreamIteratorOptionsFields
type ReadableStreamIteratorOptions =
    JSObject ReadableStreamIteratorOptionsClass
type ReifiedReadableStreamIteratorOptions =
    ReifiedDictionary ReadableStreamIteratorOptionsFields
