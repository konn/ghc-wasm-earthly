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
module GHC.Wasm.Web.Generated.MediaSessionAction.Core (
        MediaSessionActionTags, MediaSessionActionClass, MediaSessionAction
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type MediaSessionActionTags =
    '["play",
      "pause",
      "seekbackward",
      "seekforward",
      "previoustrack",
      "nexttrack",
      "skipad",
      "stop",
      "seekto",
      "togglemicrophone",
      "togglecamera",
      "hangup"]
type MediaSessionActionClass = EnumClass MediaSessionActionTags
type MediaSessionAction = JSObject MediaSessionActionClass
