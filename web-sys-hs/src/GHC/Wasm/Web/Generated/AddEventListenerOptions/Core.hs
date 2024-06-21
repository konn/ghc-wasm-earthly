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
module GHC.Wasm.Web.Generated.AddEventListenerOptions.Core (
        AddEventListenerOptionsFields, AddEventListenerOptionsClass,
        AddEventListenerOptions, ReifiedAddEventListenerOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type AddEventListenerOptionsFields =
    '[ '("capture", NullableClass (JSPrimClass Bool)),
       '("once", NullableClass (JSPrimClass Bool)),
       '("passive", NullableClass (JSPrimClass Bool))]
type AddEventListenerOptionsClass =
    JSDictionaryClass AddEventListenerOptionsFields
type AddEventListenerOptions =
    JSObject AddEventListenerOptionsClass
type ReifiedAddEventListenerOptions =
    ReifiedDictionary AddEventListenerOptionsFields
