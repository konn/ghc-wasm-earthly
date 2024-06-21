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
module GHC.Wasm.Web.Generated.ConstrainBooleanParameters.Core (
        ConstrainBooleanParametersFields, ConstrainBooleanParametersClass,
        ConstrainBooleanParameters, ReifiedConstrainBooleanParameters
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type ConstrainBooleanParametersFields =
    '[ '("exact", NullableClass (JSPrimClass Bool)),
       '("ideal", NullableClass (JSPrimClass Bool))]
type ConstrainBooleanParametersClass =
    JSDictionaryClass ConstrainBooleanParametersFields
type ConstrainBooleanParameters =
    JSObject ConstrainBooleanParametersClass
type ReifiedConstrainBooleanParameters =
    ReifiedDictionary ConstrainBooleanParametersFields
