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
module GHC.Wasm.Web.Generated.DOMPointInit.Core (
        DOMPointInitFields, DOMPointInitClass, DOMPointInit,
        ReifiedDOMPointInit
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type DOMPointInitFields =
    '[ '("w", NullableClass (JSPrimClass Double)),
       '("x", NullableClass (JSPrimClass Double)),
       '("y", NullableClass (JSPrimClass Double)),
       '("z", NullableClass (JSPrimClass Double))]
type DOMPointInitClass = JSDictionaryClass DOMPointInitFields
type DOMPointInit = JSObject DOMPointInitClass
type ReifiedDOMPointInit = ReifiedDictionary DOMPointInitFields
