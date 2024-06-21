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
module GHC.Wasm.Web.Generated.MediaError (
        MediaError, MediaErrorClass, js_const_MediaError_MEDIA_ERR_ABORTED,
        js_const_MediaError_MEDIA_ERR_NETWORK,
        js_const_MediaError_MEDIA_ERR_DECODE,
        js_const_MediaError_MEDIA_ERR_SRC_NOT_SUPPORTED, js_get_code,
        js_get_message
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MediaError.Core
import GHC.Wasm.Web.Types
js_const_MediaError_MEDIA_ERR_ABORTED :: Word16
js_const_MediaError_MEDIA_ERR_ABORTED = 1
js_const_MediaError_MEDIA_ERR_NETWORK :: Word16
js_const_MediaError_MEDIA_ERR_NETWORK = 2
js_const_MediaError_MEDIA_ERR_DECODE :: Word16
js_const_MediaError_MEDIA_ERR_DECODE = 3
js_const_MediaError_MEDIA_ERR_SRC_NOT_SUPPORTED :: Word16
js_const_MediaError_MEDIA_ERR_SRC_NOT_SUPPORTED = 4
foreign import javascript unsafe "$1.code" js_get_code
  :: MediaError -> (IO Word16)
foreign import javascript unsafe "$1.message" js_get_message
  :: MediaError -> (IO DOMString)
