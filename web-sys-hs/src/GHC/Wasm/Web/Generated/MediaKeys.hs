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
module GHC.Wasm.Web.Generated.MediaKeys (
        MediaKeys, MediaKeysClass,
        js_fun_createSession_nullable_MediaKeySessionType_MediaKeySession,
        js_fun_setServerCertificate_BufferSource_Promise_undefined,
        js_fun_getStatusForPolicy_nullable_MediaKeysPolicy_Promise_MediaKeyStatus,
        js_get_keySystem
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MediaKeySession.Core
import GHC.Wasm.Web.Generated.MediaKeySessionType.Core
import GHC.Wasm.Web.Generated.MediaKeyStatus.Core
import GHC.Wasm.Web.Generated.MediaKeys.Core
import GHC.Wasm.Web.Generated.MediaKeysPolicy.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.createSession($2)" js_fun_createSession_nullable_MediaKeySessionType_MediaKeySession
  :: MediaKeys
     -> (Nullable MediaKeySessionTypeClass -> (IO MediaKeySession))
foreign import javascript safe "$1.setServerCertificate($2)" js_fun_setServerCertificate_BufferSource_Promise_undefined
  :: MediaKeys -> (BufferSource -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.getStatusForPolicy($2)" js_fun_getStatusForPolicy_nullable_MediaKeysPolicy_Promise_MediaKeyStatus
  :: MediaKeys
     -> (Nullable MediaKeysPolicyClass
         -> (IO (Promise MediaKeyStatusClass)))
foreign import javascript unsafe "$1.keySystem" js_get_keySystem
  :: MediaKeys -> (IO DOMString)
