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
module GHC.Wasm.Web.Generated.ConstrainDOMStringParameters.Core (
        ConstrainDOMStringParametersFields,
        ConstrainDOMStringParametersClass, ConstrainDOMStringParameters,
        ReifiedConstrainDOMStringParameters
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type ConstrainDOMStringParametersFields =
    '[ '("exact",
         NullableClass (UnionClass '[DOMStringClass,
                                     SequenceClass DOMStringClass])),
       '("ideal",
         NullableClass (UnionClass '[DOMStringClass,
                                     SequenceClass DOMStringClass]))]
type ConstrainDOMStringParametersClass =
    JSDictionaryClass ConstrainDOMStringParametersFields
type ConstrainDOMStringParameters =
    JSObject ConstrainDOMStringParametersClass
type ReifiedConstrainDOMStringParameters =
    ReifiedDictionary ConstrainDOMStringParametersFields
