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
module GHC.Wasm.Web.Generated.IDBCursor (
        IDBCursor, IDBCursorClass, js_fun_update_any_IDBRequest,
        js_fun_advance_long_undefined,
        js_fun_continue_nullable_any_undefined,
        js_fun_continuePrimaryKey_any_any_undefined,
        js_fun_delete__IDBRequest, js_get_source, js_get_direction,
        js_get_key, js_get_primaryKey, js_get_request
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.IDBCursor.Core
import GHC.Wasm.Web.Generated.IDBCursorDirection.Core
import GHC.Wasm.Web.Generated.IDBIndex.Core
import GHC.Wasm.Web.Generated.IDBObjectStore.Core
import GHC.Wasm.Web.Generated.IDBRequest.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.update($2)" js_fun_update_any_IDBRequest
  :: IDBCursor -> (JSAny -> (IO IDBRequest))
foreign import javascript unsafe "$1.advance($2)" js_fun_advance_long_undefined
  :: IDBCursor -> (Word32 -> (IO ()))
foreign import javascript unsafe "$1.continue($2)" js_fun_continue_nullable_any_undefined
  :: IDBCursor -> (Nullable AnyClass -> (IO ()))
foreign import javascript unsafe "$1.continuePrimaryKey($2,$3)" js_fun_continuePrimaryKey_any_any_undefined
  :: IDBCursor -> (JSAny -> (JSAny -> (IO ())))
foreign import javascript unsafe "$1.delete()" js_fun_delete__IDBRequest
  :: IDBCursor -> (IO IDBRequest)
foreign import javascript unsafe "$1.source" js_get_source
  :: IDBCursor
     -> (IO (JSObject (UnionClass '[IDBObjectStoreClass,
                                    IDBIndexClass])))
foreign import javascript unsafe "$1.direction" js_get_direction
  :: IDBCursor -> (IO IDBCursorDirection)
foreign import javascript unsafe "$1.key" js_get_key
  :: IDBCursor -> (IO JSAny)
foreign import javascript unsafe "$1.primaryKey" js_get_primaryKey
  :: IDBCursor -> (IO JSAny)
foreign import javascript unsafe "$1.request" js_get_request
  :: IDBCursor -> (IO IDBRequest)
