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
module GHC.Wasm.Web.Generated.TextTrackCueList (
        TextTrackCueList, TextTrackCueListClass,
        js_fun_getCueById_DOMString_nullable_VTTCue, js_get_length
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.TextTrackCueList.Core
import GHC.Wasm.Web.Generated.VTTCue.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getCueById($2)" js_fun_getCueById_DOMString_nullable_VTTCue
  :: TextTrackCueList -> (DOMString -> (IO (Nullable VTTCueClass)))
foreign import javascript unsafe "$1.length" js_get_length
  :: TextTrackCueList -> (IO Word32)
