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
module GHC.Wasm.Web.Generated.SerialPort (
        SerialPort, SerialPortClass, js_fun_getInfo__SerialPortInfo,
        js_fun_open_SerialOptions_Promise_undefined,
        js_fun_setSignals_nullable_SerialOutputSignals_Promise_undefined,
        js_fun_getSignals__Promise_SerialInputSignals,
        js_fun_close__Promise_undefined, js_fun_forget__Promise_undefined,
        js_get_onconnect, js_set_onconnect, js_get_ondisconnect,
        js_set_ondisconnect, js_get_readable, js_get_writable
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.ReadableStream.Core
import GHC.Wasm.Web.Generated.SerialInputSignals.Core
import GHC.Wasm.Web.Generated.SerialOptions.Core
import GHC.Wasm.Web.Generated.SerialOutputSignals.Core
import GHC.Wasm.Web.Generated.SerialPort.Core
import GHC.Wasm.Web.Generated.SerialPortInfo.Core
import GHC.Wasm.Web.Generated.WritableStream.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getInfo()" js_fun_getInfo__SerialPortInfo
  :: SerialPort -> (IO SerialPortInfo)
foreign import javascript safe "$1.open($2)" js_fun_open_SerialOptions_Promise_undefined
  :: SerialPort -> (SerialOptions -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.setSignals($2)" js_fun_setSignals_nullable_SerialOutputSignals_Promise_undefined
  :: SerialPort
     -> (Nullable SerialOutputSignalsClass
         -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.getSignals()" js_fun_getSignals__Promise_SerialInputSignals
  :: SerialPort -> (IO (Promise SerialInputSignalsClass))
foreign import javascript safe "$1.close()" js_fun_close__Promise_undefined
  :: SerialPort -> (IO (Promise UndefinedClass))
foreign import javascript safe "$1.forget()" js_fun_forget__Promise_undefined
  :: SerialPort -> (IO (Promise UndefinedClass))
foreign import javascript unsafe "$1.onconnect" js_get_onconnect
  :: SerialPort -> (IO EventHandler)
foreign import javascript unsafe "$1.onconnect = $2" js_set_onconnect
  :: SerialPort -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondisconnect" js_get_ondisconnect
  :: SerialPort -> (IO EventHandler)
foreign import javascript unsafe "$1.ondisconnect = $2" js_set_ondisconnect
  :: SerialPort -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.readable" js_get_readable
  :: SerialPort -> (IO ReadableStream)
foreign import javascript unsafe "$1.writable" js_get_writable
  :: SerialPort -> (IO WritableStream)
