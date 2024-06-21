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
module GHC.Wasm.Web.Generated.MIDIInput (
        MIDIInput, MIDIInputClass, js_get_onmidimessage,
        js_set_onmidimessage
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.MIDIInput.Core
import GHC.Wasm.Web.Generated.MIDIPort.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.onmidimessage" js_get_onmidimessage
  :: MIDIInput -> (IO EventHandler)
foreign import javascript unsafe "$1.onmidimessage = $2" js_set_onmidimessage
  :: MIDIInput -> (EventHandler -> (IO ()))
