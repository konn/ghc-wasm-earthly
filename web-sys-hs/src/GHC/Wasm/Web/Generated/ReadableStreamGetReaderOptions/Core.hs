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
module GHC.Wasm.Web.Generated.ReadableStreamGetReaderOptions.Core (
        ReadableStreamGetReaderOptionsFields,
        ReadableStreamGetReaderOptionsClass,
        ReadableStreamGetReaderOptions,
        ReifiedReadableStreamGetReaderOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ReadableStreamReaderMode.Core
import GHC.Wasm.Web.Types
type ReadableStreamGetReaderOptionsFields =
    '[ '("mode", NullableClass ReadableStreamReaderModeClass)]
type ReadableStreamGetReaderOptionsClass =
    JSDictionaryClass ReadableStreamGetReaderOptionsFields
type ReadableStreamGetReaderOptions =
    JSObject ReadableStreamGetReaderOptionsClass
type ReifiedReadableStreamGetReaderOptions =
    ReifiedDictionary ReadableStreamGetReaderOptionsFields
