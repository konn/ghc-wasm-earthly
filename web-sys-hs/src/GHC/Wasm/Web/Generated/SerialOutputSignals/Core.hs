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
module GHC.Wasm.Web.Generated.SerialOutputSignals.Core (
        SerialOutputSignalsFields, SerialOutputSignalsClass,
        SerialOutputSignals, ReifiedSerialOutputSignals
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type SerialOutputSignalsFields =
    '[ '("break", NullableClass (JSPrimClass Bool)),
       '("dataTerminalReady", NullableClass (JSPrimClass Bool)),
       '("requestToSend", NullableClass (JSPrimClass Bool))]
type SerialOutputSignalsClass =
    JSDictionaryClass SerialOutputSignalsFields
type SerialOutputSignals = JSObject SerialOutputSignalsClass
type ReifiedSerialOutputSignals =
    ReifiedDictionary SerialOutputSignalsFields
