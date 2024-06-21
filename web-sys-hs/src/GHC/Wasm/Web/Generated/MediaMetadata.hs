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
module GHC.Wasm.Web.Generated.MediaMetadata (
        MediaMetadata, MediaMetadataClass, js_cons_MediaMetadata,
        js_get_title, js_set_title, js_get_artist, js_set_artist,
        js_get_album, js_set_album, js_get_artwork, js_set_artwork
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MediaImage.Core
import GHC.Wasm.Web.Generated.MediaMetadata.Core
import GHC.Wasm.Web.Generated.MediaMetadataInit.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new MediaMetadata($1)" js_cons_MediaMetadata
  :: Nullable MediaMetadataInitClass -> (IO MediaMetadata)
foreign import javascript unsafe "$1.title" js_get_title
  :: MediaMetadata -> (IO DOMString)
foreign import javascript unsafe "$1.title = $2" js_set_title
  :: MediaMetadata -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.artist" js_get_artist
  :: MediaMetadata -> (IO DOMString)
foreign import javascript unsafe "$1.artist = $2" js_set_artist
  :: MediaMetadata -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.album" js_get_album
  :: MediaMetadata -> (IO DOMString)
foreign import javascript unsafe "$1.album = $2" js_set_album
  :: MediaMetadata -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.artwork" js_get_artwork
  :: MediaMetadata -> (IO (FrozenArray MediaImageClass))
foreign import javascript unsafe "$1.artwork = $2" js_set_artwork
  :: MediaMetadata -> (FrozenArray MediaImageClass -> (IO ()))
