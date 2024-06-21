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
module GHC.Wasm.Web.Generated.DOMException (
        DOMException, DOMExceptionClass, js_cons_DOMException,
        js_const_DOMException_INDEX_SIZE_ERR,
        js_const_DOMException_DOMSTRING_SIZE_ERR,
        js_const_DOMException_HIERARCHY_REQUEST_ERR,
        js_const_DOMException_WRONG_DOCUMENT_ERR,
        js_const_DOMException_INVALID_CHARACTER_ERR,
        js_const_DOMException_NO_DATA_ALLOWED_ERR,
        js_const_DOMException_NO_MODIFICATION_ALLOWED_ERR,
        js_const_DOMException_NOT_FOUND_ERR,
        js_const_DOMException_NOT_SUPPORTED_ERR,
        js_const_DOMException_INUSE_ATTRIBUTE_ERR,
        js_const_DOMException_INVALID_STATE_ERR,
        js_const_DOMException_SYNTAX_ERR,
        js_const_DOMException_INVALID_MODIFICATION_ERR,
        js_const_DOMException_NAMESPACE_ERR,
        js_const_DOMException_INVALID_ACCESS_ERR,
        js_const_DOMException_VALIDATION_ERR,
        js_const_DOMException_TYPE_MISMATCH_ERR,
        js_const_DOMException_SECURITY_ERR,
        js_const_DOMException_NETWORK_ERR, js_const_DOMException_ABORT_ERR,
        js_const_DOMException_URL_MISMATCH_ERR,
        js_const_DOMException_QUOTA_EXCEEDED_ERR,
        js_const_DOMException_TIMEOUT_ERR,
        js_const_DOMException_INVALID_NODE_TYPE_ERR,
        js_const_DOMException_DATA_CLONE_ERR, js_get_name, js_get_message,
        js_get_code, js_get_result, js_get_filename, js_get_lineNumber,
        js_get_columnNumber, js_get_location, js_get_data, js_get_stack
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMException.Core
import GHC.Wasm.Web.Generated.NsISupports.Core
import GHC.Wasm.Web.Generated.StackFrame.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new DOMException($1,$2)" js_cons_DOMException
  :: Nullable DOMStringClass
     -> (Nullable DOMStringClass -> (IO DOMException))
js_const_DOMException_INDEX_SIZE_ERR :: Word16
js_const_DOMException_INDEX_SIZE_ERR = 1
js_const_DOMException_DOMSTRING_SIZE_ERR :: Word16
js_const_DOMException_DOMSTRING_SIZE_ERR = 2
js_const_DOMException_HIERARCHY_REQUEST_ERR :: Word16
js_const_DOMException_HIERARCHY_REQUEST_ERR = 3
js_const_DOMException_WRONG_DOCUMENT_ERR :: Word16
js_const_DOMException_WRONG_DOCUMENT_ERR = 4
js_const_DOMException_INVALID_CHARACTER_ERR :: Word16
js_const_DOMException_INVALID_CHARACTER_ERR = 5
js_const_DOMException_NO_DATA_ALLOWED_ERR :: Word16
js_const_DOMException_NO_DATA_ALLOWED_ERR = 6
js_const_DOMException_NO_MODIFICATION_ALLOWED_ERR :: Word16
js_const_DOMException_NO_MODIFICATION_ALLOWED_ERR = 7
js_const_DOMException_NOT_FOUND_ERR :: Word16
js_const_DOMException_NOT_FOUND_ERR = 8
js_const_DOMException_NOT_SUPPORTED_ERR :: Word16
js_const_DOMException_NOT_SUPPORTED_ERR = 9
js_const_DOMException_INUSE_ATTRIBUTE_ERR :: Word16
js_const_DOMException_INUSE_ATTRIBUTE_ERR = 10
js_const_DOMException_INVALID_STATE_ERR :: Word16
js_const_DOMException_INVALID_STATE_ERR = 11
js_const_DOMException_SYNTAX_ERR :: Word16
js_const_DOMException_SYNTAX_ERR = 12
js_const_DOMException_INVALID_MODIFICATION_ERR :: Word16
js_const_DOMException_INVALID_MODIFICATION_ERR = 13
js_const_DOMException_NAMESPACE_ERR :: Word16
js_const_DOMException_NAMESPACE_ERR = 14
js_const_DOMException_INVALID_ACCESS_ERR :: Word16
js_const_DOMException_INVALID_ACCESS_ERR = 15
js_const_DOMException_VALIDATION_ERR :: Word16
js_const_DOMException_VALIDATION_ERR = 16
js_const_DOMException_TYPE_MISMATCH_ERR :: Word16
js_const_DOMException_TYPE_MISMATCH_ERR = 17
js_const_DOMException_SECURITY_ERR :: Word16
js_const_DOMException_SECURITY_ERR = 18
js_const_DOMException_NETWORK_ERR :: Word16
js_const_DOMException_NETWORK_ERR = 19
js_const_DOMException_ABORT_ERR :: Word16
js_const_DOMException_ABORT_ERR = 20
js_const_DOMException_URL_MISMATCH_ERR :: Word16
js_const_DOMException_URL_MISMATCH_ERR = 21
js_const_DOMException_QUOTA_EXCEEDED_ERR :: Word16
js_const_DOMException_QUOTA_EXCEEDED_ERR = 22
js_const_DOMException_TIMEOUT_ERR :: Word16
js_const_DOMException_TIMEOUT_ERR = 23
js_const_DOMException_INVALID_NODE_TYPE_ERR :: Word16
js_const_DOMException_INVALID_NODE_TYPE_ERR = 24
js_const_DOMException_DATA_CLONE_ERR :: Word16
js_const_DOMException_DATA_CLONE_ERR = 25
foreign import javascript unsafe "$1.name" js_get_name
  :: DOMException -> (IO DOMString)
foreign import javascript unsafe "$1.message" js_get_message
  :: DOMException -> (IO DOMString)
foreign import javascript unsafe "$1.code" js_get_code
  :: DOMException -> (IO Word16)
foreign import javascript unsafe "$1.result" js_get_result
  :: DOMException -> (IO Word32)
foreign import javascript unsafe "$1.filename" js_get_filename
  :: DOMException -> (IO DOMString)
foreign import javascript unsafe "$1.lineNumber" js_get_lineNumber
  :: DOMException -> (IO Word32)
foreign import javascript unsafe "$1.columnNumber" js_get_columnNumber
  :: DOMException -> (IO Word32)
foreign import javascript unsafe "$1.location" js_get_location
  :: DOMException -> (IO (Nullable StackFrameClass))
foreign import javascript unsafe "$1.data" js_get_data
  :: DOMException -> (IO (Nullable NsISupportsClass))
foreign import javascript unsafe "$1.stack" js_get_stack
  :: DOMException -> (IO DOMString)
