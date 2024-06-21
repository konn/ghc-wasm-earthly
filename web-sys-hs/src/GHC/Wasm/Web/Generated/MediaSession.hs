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
module GHC.Wasm.Web.Generated.MediaSession (
        MediaSession, MediaSessionClass,
        js_fun_setActionHandler_MediaSessionAction_nullable_MediaSessionActionHandler_undefined,
        js_fun_setPositionState_nullable_MediaPositionState_undefined,
        js_fun_setMicrophoneActive_boolean_undefined,
        js_fun_setCameraActive_boolean_undefined, js_get_metadata,
        js_set_metadata, js_get_playbackState, js_set_playbackState
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MediaMetadata.Core
import GHC.Wasm.Web.Generated.MediaPositionState.Core
import GHC.Wasm.Web.Generated.MediaSession.Core
import GHC.Wasm.Web.Generated.MediaSessionAction.Core
import GHC.Wasm.Web.Generated.MediaSessionActionHandler.Core
import GHC.Wasm.Web.Generated.MediaSessionPlaybackState.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.setActionHandler($2,$3)" js_fun_setActionHandler_MediaSessionAction_nullable_MediaSessionActionHandler_undefined
  :: MediaSession
     -> (MediaSessionAction
         -> (Nullable MediaSessionActionHandlerClass -> (IO ())))
foreign import javascript unsafe "$1.setPositionState($2)" js_fun_setPositionState_nullable_MediaPositionState_undefined
  :: MediaSession -> (Nullable MediaPositionStateClass -> (IO ()))
foreign import javascript unsafe "$1.setMicrophoneActive($2)" js_fun_setMicrophoneActive_boolean_undefined
  :: MediaSession -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.setCameraActive($2)" js_fun_setCameraActive_boolean_undefined
  :: MediaSession -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.metadata" js_get_metadata
  :: MediaSession -> (IO (Nullable MediaMetadataClass))
foreign import javascript unsafe "$1.metadata = $2" js_set_metadata
  :: MediaSession -> (Nullable MediaMetadataClass -> (IO ()))
foreign import javascript unsafe "$1.playbackState" js_get_playbackState
  :: MediaSession -> (IO MediaSessionPlaybackState)
foreign import javascript unsafe "$1.playbackState = $2" js_set_playbackState
  :: MediaSession -> (MediaSessionPlaybackState -> (IO ()))
