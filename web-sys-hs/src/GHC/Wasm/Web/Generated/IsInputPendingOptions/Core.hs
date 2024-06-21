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
module GHC.Wasm.Web.Generated.IsInputPendingOptions.Core (
        IsInputPendingOptionsFields, IsInputPendingOptionsClass,
        IsInputPendingOptions, ReifiedIsInputPendingOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type IsInputPendingOptionsFields =
    '[ '("includeContinuous", NullableClass (JSPrimClass Bool))]
type IsInputPendingOptionsClass =
    JSDictionaryClass IsInputPendingOptionsFields
type IsInputPendingOptions = JSObject IsInputPendingOptionsClass
type ReifiedIsInputPendingOptions =
    ReifiedDictionary IsInputPendingOptionsFields
