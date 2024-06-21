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
module GHC.Wasm.Web.Generated.ConstrainDoubleRange.Core (
        ConstrainDoubleRangeFields, ConstrainDoubleRangeClass,
        ConstrainDoubleRange, ReifiedConstrainDoubleRange
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type ConstrainDoubleRangeFields =
    '[ '("exact", NullableClass (JSPrimClass Double)),
       '("ideal", NullableClass (JSPrimClass Double)),
       '("max", NullableClass (JSPrimClass Double)),
       '("min", NullableClass (JSPrimClass Double))]
type ConstrainDoubleRangeClass =
    JSDictionaryClass ConstrainDoubleRangeFields
type ConstrainDoubleRange = JSObject ConstrainDoubleRangeClass
type ReifiedConstrainDoubleRange =
    ReifiedDictionary ConstrainDoubleRangeFields
