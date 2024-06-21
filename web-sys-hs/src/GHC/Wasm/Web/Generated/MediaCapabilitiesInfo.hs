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
module GHC.Wasm.Web.Generated.MediaCapabilitiesInfo (
        MediaCapabilitiesInfo, MediaCapabilitiesInfoClass,
        js_get_supported, js_get_smooth, js_get_powerEfficient
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MediaCapabilitiesInfo.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.supported" js_get_supported
  :: MediaCapabilitiesInfo -> (IO Bool)
foreign import javascript unsafe "$1.smooth" js_get_smooth
  :: MediaCapabilitiesInfo -> (IO Bool)
foreign import javascript unsafe "$1.powerEfficient" js_get_powerEfficient
  :: MediaCapabilitiesInfo -> (IO Bool)
