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
module GHC.Wasm.Web.Generated.PositionOptions.Core (
        PositionOptionsFields, PositionOptionsClass, PositionOptions,
        ReifiedPositionOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type PositionOptionsFields =
    '[ '("enableHighAccuracy", NullableClass (JSPrimClass Bool)),
       '("maximumAge", NullableClass (JSPrimClass Word32)),
       '("timeout", NullableClass (JSPrimClass Word32))]
type PositionOptionsClass = JSDictionaryClass PositionOptionsFields
type PositionOptions = JSObject PositionOptionsClass
type ReifiedPositionOptions =
    ReifiedDictionary PositionOptionsFields
