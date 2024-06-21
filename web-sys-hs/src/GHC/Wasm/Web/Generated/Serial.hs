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
module GHC.Wasm.Web.Generated.Serial (
        Serial, SerialClass, js_fun_getPorts__Promise_sequence_SerialPort,
        js_fun_requestPort_nullable_SerialPortRequestOptions_Promise_SerialPort,
        js_get_onconnect, js_set_onconnect, js_get_ondisconnect,
        js_set_ondisconnect
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.Serial.Core
import GHC.Wasm.Web.Generated.SerialPort.Core
import GHC.Wasm.Web.Generated.SerialPortRequestOptions.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.getPorts()" js_fun_getPorts__Promise_sequence_SerialPort
  :: Serial -> (IO (Promise (SequenceClass SerialPortClass)))
foreign import javascript safe "$1.requestPort($2)" js_fun_requestPort_nullable_SerialPortRequestOptions_Promise_SerialPort
  :: Serial
     -> (Nullable SerialPortRequestOptionsClass
         -> (IO (Promise SerialPortClass)))
foreign import javascript unsafe "$1.onconnect" js_get_onconnect
  :: Serial -> (IO EventHandler)
foreign import javascript unsafe "$1.onconnect = $2" js_set_onconnect
  :: Serial -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondisconnect" js_get_ondisconnect
  :: Serial -> (IO EventHandler)
foreign import javascript unsafe "$1.ondisconnect = $2" js_set_ondisconnect
  :: Serial -> (EventHandler -> (IO ()))
