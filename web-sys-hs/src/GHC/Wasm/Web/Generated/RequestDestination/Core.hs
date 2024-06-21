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
module GHC.Wasm.Web.Generated.RequestDestination.Core (
        RequestDestinationTags, RequestDestinationClass, RequestDestination
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type RequestDestinationTags =
    '["",
      "audio",
      "audioworklet",
      "document",
      "embed",
      "font",
      "image",
      "manifest",
      "object",
      "paintworklet",
      "report",
      "script",
      "sharedworker",
      "style",
      "track",
      "video",
      "worker",
      "xslt"]
type RequestDestinationClass = EnumClass RequestDestinationTags
type RequestDestination = JSObject RequestDestinationClass
