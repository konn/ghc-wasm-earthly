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
module GHC.Wasm.Web.Generated.SpeechSynthesisVoice (
        SpeechSynthesisVoice, SpeechSynthesisVoiceClass, js_get_voiceURI,
        js_get_name, js_get_lang, js_get_localService, js_get_default
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.SpeechSynthesisVoice.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.voiceURI" js_get_voiceURI
  :: SpeechSynthesisVoice -> (IO DOMString)
foreign import javascript unsafe "$1.name" js_get_name
  :: SpeechSynthesisVoice -> (IO DOMString)
foreign import javascript unsafe "$1.lang" js_get_lang
  :: SpeechSynthesisVoice -> (IO DOMString)
foreign import javascript unsafe "$1.localService" js_get_localService
  :: SpeechSynthesisVoice -> (IO Bool)
foreign import javascript unsafe "$1.default" js_get_default
  :: SpeechSynthesisVoice -> (IO Bool)
