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
module GHC.Wasm.Web.Generated.DOMRectInit.Core (
        DOMRectInitFields, DOMRectInitClass, DOMRectInit,
        ReifiedDOMRectInit
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type DOMRectInitFields =
    '[ '("height", NullableClass (JSPrimClass Double)),
       '("width", NullableClass (JSPrimClass Double)),
       '("x", NullableClass (JSPrimClass Double)),
       '("y", NullableClass (JSPrimClass Double))]
type DOMRectInitClass = JSDictionaryClass DOMRectInitFields
type DOMRectInit = JSObject DOMRectInitClass
type ReifiedDOMRectInit = ReifiedDictionary DOMRectInitFields
