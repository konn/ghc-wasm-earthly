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
module GHC.Wasm.Web.Generated.MIDIPort (
        MIDIPort, MIDIPortClass, js_fun_open__Promise_MIDIPort,
        js_fun_close__Promise_MIDIPort, js_get_id, js_get_manufacturer,
        js_get_name, js_get_version, js_get_type, js_get_state,
        js_get_connection, js_get_onstatechange, js_set_onstatechange
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.MIDIPort.Core
import GHC.Wasm.Web.Generated.MIDIPortConnectionState.Core
import GHC.Wasm.Web.Generated.MIDIPortDeviceState.Core
import GHC.Wasm.Web.Generated.MIDIPortType.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.open()" js_fun_open__Promise_MIDIPort
  :: MIDIPort -> (IO (Promise MIDIPortClass))
foreign import javascript safe "$1.close()" js_fun_close__Promise_MIDIPort
  :: MIDIPort -> (IO (Promise MIDIPortClass))
foreign import javascript unsafe "$1.id" js_get_id
  :: MIDIPort -> (IO DOMString)
foreign import javascript unsafe "$1.manufacturer" js_get_manufacturer
  :: MIDIPort -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.name" js_get_name
  :: MIDIPort -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.version" js_get_version
  :: MIDIPort -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.type" js_get_type
  :: MIDIPort -> (IO MIDIPortType)
foreign import javascript unsafe "$1.state" js_get_state
  :: MIDIPort -> (IO MIDIPortDeviceState)
foreign import javascript unsafe "$1.connection" js_get_connection
  :: MIDIPort -> (IO MIDIPortConnectionState)
foreign import javascript unsafe "$1.onstatechange" js_get_onstatechange
  :: MIDIPort -> (IO EventHandler)
foreign import javascript unsafe "$1.onstatechange = $2" js_set_onstatechange
  :: MIDIPort -> (EventHandler -> (IO ()))
