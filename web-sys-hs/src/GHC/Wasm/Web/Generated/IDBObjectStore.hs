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
module GHC.Wasm.Web.Generated.IDBObjectStore (
        IDBObjectStore, IDBObjectStoreClass,
        js_fun_put_any_nullable_any_IDBRequest,
        js_fun_add_any_nullable_any_IDBRequest,
        js_fun_delete_any_IDBRequest, js_fun_get_any_IDBRequest,
        js_fun_getKey_any_IDBRequest, js_fun_clear__IDBRequest,
        js_fun_openCursor_nullable_any_nullable_IDBCursorDirection_IDBRequest,
        js_fun_createIndex_DOMString_Union_DOMString_sequence_DOMString_EndUnion_nullable_IDBIndexParameters_IDBIndex,
        js_fun_index_DOMString_IDBIndex,
        js_fun_deleteIndex_DOMString_undefined,
        js_fun_count_nullable_any_IDBRequest,
        js_fun_getAll_nullable_any_nullable_long_IDBRequest,
        js_fun_getAllKeys_nullable_any_nullable_long_IDBRequest,
        js_fun_openKeyCursor_nullable_any_nullable_IDBCursorDirection_IDBRequest,
        js_get_name, js_set_name, js_get_keyPath, js_get_indexNames,
        js_get_transaction, js_get_autoIncrement
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMStringList.Core
import GHC.Wasm.Web.Generated.IDBCursorDirection.Core
import GHC.Wasm.Web.Generated.IDBIndex.Core
import GHC.Wasm.Web.Generated.IDBIndexParameters.Core
import GHC.Wasm.Web.Generated.IDBObjectStore.Core
import GHC.Wasm.Web.Generated.IDBRequest.Core
import GHC.Wasm.Web.Generated.IDBTransaction.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.put($2,$3)" js_fun_put_any_nullable_any_IDBRequest
  :: IDBObjectStore
     -> (JSAny -> (Nullable AnyClass -> (IO IDBRequest)))
foreign import javascript unsafe "$1.add($2,$3)" js_fun_add_any_nullable_any_IDBRequest
  :: IDBObjectStore
     -> (JSAny -> (Nullable AnyClass -> (IO IDBRequest)))
foreign import javascript unsafe "$1.delete($2)" js_fun_delete_any_IDBRequest
  :: IDBObjectStore -> (JSAny -> (IO IDBRequest))
foreign import javascript unsafe "$1.get($2)" js_fun_get_any_IDBRequest
  :: IDBObjectStore -> (JSAny -> (IO IDBRequest))
foreign import javascript unsafe "$1.getKey($2)" js_fun_getKey_any_IDBRequest
  :: IDBObjectStore -> (JSAny -> (IO IDBRequest))
foreign import javascript unsafe "$1.clear()" js_fun_clear__IDBRequest
  :: IDBObjectStore -> (IO IDBRequest)
foreign import javascript unsafe "$1.openCursor($2,$3)" js_fun_openCursor_nullable_any_nullable_IDBCursorDirection_IDBRequest
  :: IDBObjectStore
     -> (Nullable AnyClass
         -> (Nullable IDBCursorDirectionClass -> (IO IDBRequest)))
foreign import javascript unsafe "$1.createIndex($2,$3,$4)" js_fun_createIndex_DOMString_Union_DOMString_sequence_DOMString_EndUnion_nullable_IDBIndexParameters_IDBIndex
  :: IDBObjectStore
     -> (DOMString
         -> (JSObject (UnionClass '[DOMStringClass,
                                    SequenceClass DOMStringClass])
             -> (Nullable IDBIndexParametersClass -> (IO IDBIndex))))
foreign import javascript unsafe "$1.index($2)" js_fun_index_DOMString_IDBIndex
  :: IDBObjectStore -> (DOMString -> (IO IDBIndex))
foreign import javascript unsafe "$1.deleteIndex($2)" js_fun_deleteIndex_DOMString_undefined
  :: IDBObjectStore -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.count($2)" js_fun_count_nullable_any_IDBRequest
  :: IDBObjectStore -> (Nullable AnyClass -> (IO IDBRequest))
foreign import javascript unsafe "$1.getAll($2,$3)" js_fun_getAll_nullable_any_nullable_long_IDBRequest
  :: IDBObjectStore
     -> (Nullable AnyClass
         -> (Nullable (JSPrimClass Word32) -> (IO IDBRequest)))
foreign import javascript unsafe "$1.getAllKeys($2,$3)" js_fun_getAllKeys_nullable_any_nullable_long_IDBRequest
  :: IDBObjectStore
     -> (Nullable AnyClass
         -> (Nullable (JSPrimClass Word32) -> (IO IDBRequest)))
foreign import javascript unsafe "$1.openKeyCursor($2,$3)" js_fun_openKeyCursor_nullable_any_nullable_IDBCursorDirection_IDBRequest
  :: IDBObjectStore
     -> (Nullable AnyClass
         -> (Nullable IDBCursorDirectionClass -> (IO IDBRequest)))
foreign import javascript unsafe "$1.name" js_get_name
  :: IDBObjectStore -> (IO DOMString)
foreign import javascript unsafe "$1.name = $2" js_set_name
  :: IDBObjectStore -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.keyPath" js_get_keyPath
  :: IDBObjectStore -> (IO JSAny)
foreign import javascript unsafe "$1.indexNames" js_get_indexNames
  :: IDBObjectStore -> (IO DOMStringList)
foreign import javascript unsafe "$1.transaction" js_get_transaction
  :: IDBObjectStore -> (IO IDBTransaction)
foreign import javascript unsafe "$1.autoIncrement" js_get_autoIncrement
  :: IDBObjectStore -> (IO Bool)
