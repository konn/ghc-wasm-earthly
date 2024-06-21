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
module GHC.Wasm.Web.Generated.TextTrackList (
        TextTrackList, TextTrackListClass,
        js_fun_getTrackById_DOMString_nullable_TextTrack, js_get_length,
        js_get_onchange, js_set_onchange, js_get_onaddtrack,
        js_set_onaddtrack, js_get_onremovetrack, js_set_onremovetrack
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.TextTrack.Core
import GHC.Wasm.Web.Generated.TextTrackList.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getTrackById($2)" js_fun_getTrackById_DOMString_nullable_TextTrack
  :: TextTrackList -> (DOMString -> (IO (Nullable TextTrackClass)))
foreign import javascript unsafe "$1.length" js_get_length
  :: TextTrackList -> (IO Word32)
foreign import javascript unsafe "$1.onchange" js_get_onchange
  :: TextTrackList -> (IO EventHandler)
foreign import javascript unsafe "$1.onchange = $2" js_set_onchange
  :: TextTrackList -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onaddtrack" js_get_onaddtrack
  :: TextTrackList -> (IO EventHandler)
foreign import javascript unsafe "$1.onaddtrack = $2" js_set_onaddtrack
  :: TextTrackList -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onremovetrack" js_get_onremovetrack
  :: TextTrackList -> (IO EventHandler)
foreign import javascript unsafe "$1.onremovetrack = $2" js_set_onremovetrack
  :: TextTrackList -> (EventHandler -> (IO ()))
