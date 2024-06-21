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
module GHC.Wasm.Web.Generated.SerialInputSignals.Core (
        SerialInputSignalsFields, SerialInputSignalsClass,
        SerialInputSignals, ReifiedSerialInputSignals
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type SerialInputSignalsFields =
    '[ '("clearToSend", JSPrimClass Bool),
       '("dataCarrierDetect", JSPrimClass Bool),
       '("dataSetReady", JSPrimClass Bool),
       '("ringIndicator", JSPrimClass Bool)]
type SerialInputSignalsClass =
    JSDictionaryClass SerialInputSignalsFields
type SerialInputSignals = JSObject SerialInputSignalsClass
type ReifiedSerialInputSignals =
    ReifiedDictionary SerialInputSignalsFields
