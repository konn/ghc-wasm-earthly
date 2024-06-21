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
module GHC.Wasm.Web.Generated.SpeechSynthesisUtterance (
        SpeechSynthesisUtterance, SpeechSynthesisUtteranceClass,
        js_cons_SpeechSynthesisUtterance, js_get_text, js_set_text,
        js_get_lang, js_set_lang, js_get_voice, js_set_voice,
        js_get_volume, js_set_volume, js_get_rate, js_set_rate,
        js_get_pitch, js_set_pitch, js_get_onstart, js_set_onstart,
        js_get_onend, js_set_onend, js_get_onerror, js_set_onerror,
        js_get_onpause, js_set_onpause, js_get_onresume, js_set_onresume,
        js_get_onmark, js_set_onmark, js_get_onboundary, js_set_onboundary,
        js_get_chosenVoiceURI
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.SpeechSynthesisUtterance.Core
import GHC.Wasm.Web.Generated.SpeechSynthesisVoice.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new SpeechSynthesisUtterance($1)" js_cons_SpeechSynthesisUtterance
  :: DOMString -> (IO SpeechSynthesisUtterance)
foreign import javascript unsafe "$1.text" js_get_text
  :: SpeechSynthesisUtterance -> (IO DOMString)
foreign import javascript unsafe "$1.text = $2" js_set_text
  :: SpeechSynthesisUtterance -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.lang" js_get_lang
  :: SpeechSynthesisUtterance -> (IO DOMString)
foreign import javascript unsafe "$1.lang = $2" js_set_lang
  :: SpeechSynthesisUtterance -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.voice" js_get_voice
  :: SpeechSynthesisUtterance
     -> (IO (Nullable SpeechSynthesisVoiceClass))
foreign import javascript unsafe "$1.voice = $2" js_set_voice
  :: SpeechSynthesisUtterance
     -> (Nullable SpeechSynthesisVoiceClass -> (IO ()))
foreign import javascript unsafe "$1.volume" js_get_volume
  :: SpeechSynthesisUtterance -> (IO Float)
foreign import javascript unsafe "$1.volume = $2" js_set_volume
  :: SpeechSynthesisUtterance -> (Float -> (IO ()))
foreign import javascript unsafe "$1.rate" js_get_rate
  :: SpeechSynthesisUtterance -> (IO Float)
foreign import javascript unsafe "$1.rate = $2" js_set_rate
  :: SpeechSynthesisUtterance -> (Float -> (IO ()))
foreign import javascript unsafe "$1.pitch" js_get_pitch
  :: SpeechSynthesisUtterance -> (IO Float)
foreign import javascript unsafe "$1.pitch = $2" js_set_pitch
  :: SpeechSynthesisUtterance -> (Float -> (IO ()))
foreign import javascript unsafe "$1.onstart" js_get_onstart
  :: SpeechSynthesisUtterance -> (IO EventHandler)
foreign import javascript unsafe "$1.onstart = $2" js_set_onstart
  :: SpeechSynthesisUtterance -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onend" js_get_onend
  :: SpeechSynthesisUtterance -> (IO EventHandler)
foreign import javascript unsafe "$1.onend = $2" js_set_onend
  :: SpeechSynthesisUtterance -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onerror" js_get_onerror
  :: SpeechSynthesisUtterance -> (IO EventHandler)
foreign import javascript unsafe "$1.onerror = $2" js_set_onerror
  :: SpeechSynthesisUtterance -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpause" js_get_onpause
  :: SpeechSynthesisUtterance -> (IO EventHandler)
foreign import javascript unsafe "$1.onpause = $2" js_set_onpause
  :: SpeechSynthesisUtterance -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onresume" js_get_onresume
  :: SpeechSynthesisUtterance -> (IO EventHandler)
foreign import javascript unsafe "$1.onresume = $2" js_set_onresume
  :: SpeechSynthesisUtterance -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmark" js_get_onmark
  :: SpeechSynthesisUtterance -> (IO EventHandler)
foreign import javascript unsafe "$1.onmark = $2" js_set_onmark
  :: SpeechSynthesisUtterance -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onboundary" js_get_onboundary
  :: SpeechSynthesisUtterance -> (IO EventHandler)
foreign import javascript unsafe "$1.onboundary = $2" js_set_onboundary
  :: SpeechSynthesisUtterance -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.chosenVoiceURI" js_get_chosenVoiceURI
  :: SpeechSynthesisUtterance -> (IO DOMString)
