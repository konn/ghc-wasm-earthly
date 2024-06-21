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
module GHC.Wasm.Web.Generated.StreamPipeOptions.Core (
        StreamPipeOptionsFields, StreamPipeOptionsClass, StreamPipeOptions,
        ReifiedStreamPipeOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AbortSignal.Core
import GHC.Wasm.Web.Types
type StreamPipeOptionsFields =
    '[ '("preventAbort", NullableClass (JSPrimClass Bool)),
       '("preventCancel", NullableClass (JSPrimClass Bool)),
       '("preventClose", NullableClass (JSPrimClass Bool)),
       '("signal", NullableClass AbortSignalClass)]
type StreamPipeOptionsClass =
    JSDictionaryClass StreamPipeOptionsFields
type StreamPipeOptions = JSObject StreamPipeOptionsClass
type ReifiedStreamPipeOptions =
    ReifiedDictionary StreamPipeOptionsFields
