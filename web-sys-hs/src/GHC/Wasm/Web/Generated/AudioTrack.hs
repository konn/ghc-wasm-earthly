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
module GHC.Wasm.Web.Generated.AudioTrack (
        AudioTrack, AudioTrackClass, js_get_id, js_get_kind, js_get_label,
        js_get_language, js_get_enabled, js_set_enabled,
        js_get_sourceBuffer
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AudioTrack.Core
import GHC.Wasm.Web.Generated.SourceBuffer.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.id" js_get_id
  :: AudioTrack -> (IO DOMString)
foreign import javascript unsafe "$1.kind" js_get_kind
  :: AudioTrack -> (IO DOMString)
foreign import javascript unsafe "$1.label" js_get_label
  :: AudioTrack -> (IO DOMString)
foreign import javascript unsafe "$1.language" js_get_language
  :: AudioTrack -> (IO DOMString)
foreign import javascript unsafe "$1.enabled" js_get_enabled
  :: AudioTrack -> (IO Bool)
foreign import javascript unsafe "$1.enabled = $2" js_set_enabled
  :: AudioTrack -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.sourceBuffer" js_get_sourceBuffer
  :: AudioTrack -> (IO (Nullable SourceBufferClass))
