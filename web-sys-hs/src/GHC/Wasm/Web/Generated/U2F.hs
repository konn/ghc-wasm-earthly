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
module GHC.Wasm.Web.Generated.U2F (
        U2F, U2FClass, js_const_U2F_OK, js_const_U2F_OTHER_ERROR,
        js_const_U2F_BAD_REQUEST, js_const_U2F_CONFIGURATION_UNSUPPORTED,
        js_const_U2F_DEVICE_INELIGIBLE, js_const_U2F_TIMEOUT,
        js_fun_register_DOMString_sequence_RegisterRequest_sequence_RegisteredKey_U2FRegisterCallback_nullable_nullable_long_undefined,
        js_fun_sign_DOMString_DOMString_sequence_RegisteredKey_U2FSignCallback_nullable_nullable_long_undefined
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.RegisterRequest.Core
import GHC.Wasm.Web.Generated.RegisteredKey.Core
import GHC.Wasm.Web.Generated.U2F.Core
import GHC.Wasm.Web.Generated.U2FRegisterCallback.Core
import GHC.Wasm.Web.Generated.U2FSignCallback.Core
import GHC.Wasm.Web.Types
js_const_U2F_OK :: Word16
js_const_U2F_OK = 0
js_const_U2F_OTHER_ERROR :: Word16
js_const_U2F_OTHER_ERROR = 1
js_const_U2F_BAD_REQUEST :: Word16
js_const_U2F_BAD_REQUEST = 2
js_const_U2F_CONFIGURATION_UNSUPPORTED :: Word16
js_const_U2F_CONFIGURATION_UNSUPPORTED = 3
js_const_U2F_DEVICE_INELIGIBLE :: Word16
js_const_U2F_DEVICE_INELIGIBLE = 4
js_const_U2F_TIMEOUT :: Word16
js_const_U2F_TIMEOUT = 5
foreign import javascript unsafe "$1.register($2,$3,$4,$5,$6)" js_fun_register_DOMString_sequence_RegisterRequest_sequence_RegisteredKey_U2FRegisterCallback_nullable_nullable_long_undefined
  :: U2F
     -> (DOMString
         -> (Sequence RegisterRequestClass
             -> (Sequence RegisteredKeyClass
                 -> (U2FRegisterCallback
                     -> (Nullable (NullableClass (JSPrimClass Int32)) -> (IO ()))))))
foreign import javascript unsafe "$1.sign($2,$3,$4,$5,$6)" js_fun_sign_DOMString_DOMString_sequence_RegisteredKey_U2FSignCallback_nullable_nullable_long_undefined
  :: U2F
     -> (DOMString
         -> (DOMString
             -> (Sequence RegisteredKeyClass
                 -> (U2FSignCallback
                     -> (Nullable (NullableClass (JSPrimClass Int32)) -> (IO ()))))))
