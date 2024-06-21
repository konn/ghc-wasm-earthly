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
module GHC.Wasm.Web.Generated.ReadableWritablePair.Core (
        ReadableWritablePairFields, ReadableWritablePairClass,
        ReadableWritablePair, ReifiedReadableWritablePair
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ReadableStream.Core
import GHC.Wasm.Web.Generated.WritableStream.Core
import GHC.Wasm.Web.Types
type ReadableWritablePairFields =
    '[ '("readable", ReadableStreamClass),
       '("writable", WritableStreamClass)]
type ReadableWritablePairClass =
    JSDictionaryClass ReadableWritablePairFields
type ReadableWritablePair = JSObject ReadableWritablePairClass
type ReifiedReadableWritablePair =
    ReifiedDictionary ReadableWritablePairFields
