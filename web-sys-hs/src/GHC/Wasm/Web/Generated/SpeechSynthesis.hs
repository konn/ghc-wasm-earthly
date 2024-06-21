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
module GHC.Wasm.Web.Generated.SpeechSynthesis (
        SpeechSynthesis, SpeechSynthesisClass,
        js_fun_speak_SpeechSynthesisUtterance_undefined,
        js_fun_cancel__undefined, js_fun_pause__undefined,
        js_fun_resume__undefined,
        js_fun_getVoices__sequence_SpeechSynthesisVoice,
        js_fun_forceEnd__undefined, js_get_pending, js_get_speaking,
        js_get_paused, js_get_onvoiceschanged, js_set_onvoiceschanged
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.SpeechSynthesis.Core
import GHC.Wasm.Web.Generated.SpeechSynthesisUtterance.Core
import GHC.Wasm.Web.Generated.SpeechSynthesisVoice.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.speak($2)" js_fun_speak_SpeechSynthesisUtterance_undefined
  :: SpeechSynthesis -> (SpeechSynthesisUtterance -> (IO ()))
foreign import javascript unsafe "$1.cancel()" js_fun_cancel__undefined
  :: SpeechSynthesis -> (IO ())
foreign import javascript unsafe "$1.pause()" js_fun_pause__undefined
  :: SpeechSynthesis -> (IO ())
foreign import javascript unsafe "$1.resume()" js_fun_resume__undefined
  :: SpeechSynthesis -> (IO ())
foreign import javascript unsafe "$1.getVoices()" js_fun_getVoices__sequence_SpeechSynthesisVoice
  :: SpeechSynthesis -> (IO (Sequence SpeechSynthesisVoiceClass))
foreign import javascript unsafe "$1.forceEnd()" js_fun_forceEnd__undefined
  :: SpeechSynthesis -> (IO ())
foreign import javascript unsafe "$1.pending" js_get_pending
  :: SpeechSynthesis -> (IO Bool)
foreign import javascript unsafe "$1.speaking" js_get_speaking
  :: SpeechSynthesis -> (IO Bool)
foreign import javascript unsafe "$1.paused" js_get_paused
  :: SpeechSynthesis -> (IO Bool)
foreign import javascript unsafe "$1.onvoiceschanged" js_get_onvoiceschanged
  :: SpeechSynthesis -> (IO EventHandler)
foreign import javascript unsafe "$1.onvoiceschanged = $2" js_set_onvoiceschanged
  :: SpeechSynthesis -> (EventHandler -> (IO ()))
