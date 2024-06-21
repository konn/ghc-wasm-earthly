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
module GHC.Wasm.Web.Generated.ConstrainLongRange.Core (
        ConstrainLongRangeFields, ConstrainLongRangeClass,
        ConstrainLongRange, ReifiedConstrainLongRange
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type ConstrainLongRangeFields =
    '[ '("exact", NullableClass (JSPrimClass Int32)),
       '("ideal", NullableClass (JSPrimClass Int32)),
       '("max", NullableClass (JSPrimClass Int32)),
       '("min", NullableClass (JSPrimClass Int32))]
type ConstrainLongRangeClass =
    JSDictionaryClass ConstrainLongRangeFields
type ConstrainLongRange = JSObject ConstrainLongRangeClass
type ReifiedConstrainLongRange =
    ReifiedDictionary ConstrainLongRangeFields
