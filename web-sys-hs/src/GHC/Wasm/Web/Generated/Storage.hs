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
module GHC.Wasm.Web.Generated.Storage (
        Storage, StorageClass, js_fun_key_long_nullable_DOMString,
        js_fun_clear__undefined, js_get_length, js_get_isSessionOnly
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Storage.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.key($2)" js_fun_key_long_nullable_DOMString
  :: Storage -> (Word32 -> (IO (Nullable DOMStringClass)))
foreign import javascript unsafe "$1.clear()" js_fun_clear__undefined
  :: Storage -> (IO ())
foreign import javascript unsafe "$1.length" js_get_length
  :: Storage -> (IO Word32)
foreign import javascript unsafe "$1.isSessionOnly" js_get_isSessionOnly
  :: Storage -> (IO Bool)
