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
module GHC.Wasm.Web.Generated.Headers (
        Headers, HeadersClass, js_cons_Headers,
        js_fun_append_ByteString_ByteString_undefined,
        js_fun_delete_ByteString_undefined,
        js_fun_get_ByteString_nullable_ByteString,
        js_fun_has_ByteString_boolean,
        js_fun_set_ByteString_ByteString_undefined, js_get_guard,
        js_set_guard
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Headers.Core
import GHC.Wasm.Web.Generated.HeadersGuardEnum.Core
import GHC.Wasm.Web.Generated.HeadersInit.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new Headers($1)" js_cons_Headers
  :: Nullable HeadersInitClass -> (IO Headers)
foreign import javascript unsafe "$1.append($2,$3)" js_fun_append_ByteString_ByteString_undefined
  :: Headers -> (JSByteString -> (JSByteString -> (IO ())))
foreign import javascript unsafe "$1.delete($2)" js_fun_delete_ByteString_undefined
  :: Headers -> (JSByteString -> (IO ()))
foreign import javascript unsafe "$1.get($2)" js_fun_get_ByteString_nullable_ByteString
  :: Headers -> (JSByteString -> (IO (Nullable JSByteStringClass)))
foreign import javascript unsafe "$1.has($2)" js_fun_has_ByteString_boolean
  :: Headers -> (JSByteString -> (IO Bool))
foreign import javascript unsafe "$1.set($2,$3)" js_fun_set_ByteString_ByteString_undefined
  :: Headers -> (JSByteString -> (JSByteString -> (IO ())))
foreign import javascript unsafe "$1.guard" js_get_guard
  :: Headers -> (IO HeadersGuardEnum)
foreign import javascript unsafe "$1.guard = $2" js_set_guard
  :: Headers -> (HeadersGuardEnum -> (IO ()))
