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
module GHC.Wasm.Web.Generated.MediaCapabilities (
        MediaCapabilities, MediaCapabilitiesClass,
        js_fun_decodingInfo_MediaDecodingConfiguration_Promise_MediaCapabilitiesInfo,
        js_fun_encodingInfo_MediaEncodingConfiguration_Promise_MediaCapabilitiesInfo
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MediaCapabilities.Core
import GHC.Wasm.Web.Generated.MediaCapabilitiesInfo.Core
import GHC.Wasm.Web.Generated.MediaDecodingConfiguration.Core
import GHC.Wasm.Web.Generated.MediaEncodingConfiguration.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.decodingInfo($2)" js_fun_decodingInfo_MediaDecodingConfiguration_Promise_MediaCapabilitiesInfo
  :: MediaCapabilities
     -> (MediaDecodingConfiguration
         -> (IO (Promise MediaCapabilitiesInfoClass)))
foreign import javascript safe "$1.encodingInfo($2)" js_fun_encodingInfo_MediaEncodingConfiguration_Promise_MediaCapabilitiesInfo
  :: MediaCapabilities
     -> (MediaEncodingConfiguration
         -> (IO (Promise MediaCapabilitiesInfoClass)))
