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
module GHC.Wasm.Web.Generated.MIDIAccess (
        MIDIAccess, MIDIAccessClass, js_get_inputs, js_get_outputs,
        js_get_onstatechange, js_set_onstatechange, js_get_sysexEnabled
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.MIDIAccess.Core
import GHC.Wasm.Web.Generated.MIDIInputMap.Core
import GHC.Wasm.Web.Generated.MIDIOutputMap.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.inputs" js_get_inputs
  :: MIDIAccess -> (IO MIDIInputMap)
foreign import javascript unsafe "$1.outputs" js_get_outputs
  :: MIDIAccess -> (IO MIDIOutputMap)
foreign import javascript unsafe "$1.onstatechange" js_get_onstatechange
  :: MIDIAccess -> (IO EventHandler)
foreign import javascript unsafe "$1.onstatechange = $2" js_set_onstatechange
  :: MIDIAccess -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.sysexEnabled" js_get_sysexEnabled
  :: MIDIAccess -> (IO Bool)
