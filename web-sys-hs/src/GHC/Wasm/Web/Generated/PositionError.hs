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
module GHC.Wasm.Web.Generated.PositionError (
        PositionError, PositionErrorClass,
        js_const_PositionError_PERMISSION_DENIED,
        js_const_PositionError_POSITION_UNAVAILABLE,
        js_const_PositionError_TIMEOUT, js_get_code, js_get_message
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.PositionError.Core
import GHC.Wasm.Web.Types
js_const_PositionError_PERMISSION_DENIED :: Word16
js_const_PositionError_PERMISSION_DENIED = 1
js_const_PositionError_POSITION_UNAVAILABLE :: Word16
js_const_PositionError_POSITION_UNAVAILABLE = 2
js_const_PositionError_TIMEOUT :: Word16
js_const_PositionError_TIMEOUT = 3
foreign import javascript unsafe "$1.code" js_get_code
  :: PositionError -> (IO Word16)
foreign import javascript unsafe "$1.message" js_get_message
  :: PositionError -> (IO DOMString)
