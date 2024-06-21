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
module GHC.Wasm.Web.Generated.Crypto (
        Crypto, CryptoClass,
        js_fun_getRandomValues_ArrayBufferView_ArrayBufferView,
        js_fun_randomUUID__DOMString, js_get_subtle
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Crypto.Core
import GHC.Wasm.Web.Generated.SubtleCrypto.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getRandomValues($2)" js_fun_getRandomValues_ArrayBufferView_ArrayBufferView
  :: Crypto -> (ArrayBufferView -> (IO ArrayBufferView))
foreign import javascript unsafe "$1.randomUUID()" js_fun_randomUUID__DOMString
  :: Crypto -> (IO DOMString)
foreign import javascript unsafe "$1.subtle" js_get_subtle
  :: Crypto -> (IO SubtleCrypto)
