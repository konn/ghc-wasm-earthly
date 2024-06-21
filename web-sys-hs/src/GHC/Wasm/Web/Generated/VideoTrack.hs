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
module GHC.Wasm.Web.Generated.VideoTrack (
        VideoTrack, VideoTrackClass, js_get_id, js_get_kind, js_get_label,
        js_get_language, js_get_selected, js_set_selected,
        js_get_sourceBuffer
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.SourceBuffer.Core
import GHC.Wasm.Web.Generated.VideoTrack.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.id" js_get_id
  :: VideoTrack -> (IO DOMString)
foreign import javascript unsafe "$1.kind" js_get_kind
  :: VideoTrack -> (IO DOMString)
foreign import javascript unsafe "$1.label" js_get_label
  :: VideoTrack -> (IO DOMString)
foreign import javascript unsafe "$1.language" js_get_language
  :: VideoTrack -> (IO DOMString)
foreign import javascript unsafe "$1.selected" js_get_selected
  :: VideoTrack -> (IO Bool)
foreign import javascript unsafe "$1.selected = $2" js_set_selected
  :: VideoTrack -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.sourceBuffer" js_get_sourceBuffer
  :: VideoTrack -> (IO (Nullable SourceBufferClass))
