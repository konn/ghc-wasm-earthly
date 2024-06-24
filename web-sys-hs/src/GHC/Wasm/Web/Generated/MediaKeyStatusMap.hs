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
module GHC.Wasm.Web.Generated.MediaKeyStatusMap (
        MediaKeyStatusMap, MediaKeyStatusMapClass,
        js_fun_has_BufferSource_boolean, js_fun_get_BufferSource_any,
        js_get_size, js_iter_MediaKeyStatusMap_ArrayBuffer_MediaKeyStatus
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MediaKeyStatus.Core
import GHC.Wasm.Web.Generated.MediaKeyStatusMap.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.has($2)" js_fun_has_BufferSource_boolean
  :: MediaKeyStatusMap -> (BufferSource -> (IO Bool))
foreign import javascript unsafe "$1.get($2)" js_fun_get_BufferSource_any
  :: MediaKeyStatusMap -> (BufferSource -> (IO JSAny))
foreign import javascript unsafe "$1.size" js_get_size
  :: MediaKeyStatusMap -> (IO Word32)
foreign import javascript unsafe "$1[Symbol.iterator]()" js_iter_MediaKeyStatusMap_ArrayBuffer_MediaKeyStatus
  :: MediaKeyStatusMap
     -> (IO (PairIterable ArrayBufferClass MediaKeyStatusClass))
