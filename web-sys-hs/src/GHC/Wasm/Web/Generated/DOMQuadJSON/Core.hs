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
module GHC.Wasm.Web.Generated.DOMQuadJSON.Core (
        DOMQuadJSONFields, DOMQuadJSONClass, DOMQuadJSON,
        ReifiedDOMQuadJSON
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMPoint.Core
import GHC.Wasm.Web.Types
type DOMQuadJSONFields =
    '[ '("p1", NullableClass DOMPointClass),
       '("p2", NullableClass DOMPointClass),
       '("p3", NullableClass DOMPointClass),
       '("p4", NullableClass DOMPointClass)]
type DOMQuadJSONClass = JSDictionaryClass DOMQuadJSONFields
type DOMQuadJSON = JSObject DOMQuadJSONClass
type ReifiedDOMQuadJSON = ReifiedDictionary DOMQuadJSONFields
