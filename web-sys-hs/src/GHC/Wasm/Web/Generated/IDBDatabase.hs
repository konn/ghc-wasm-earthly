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
module GHC.Wasm.Web.Generated.IDBDatabase (
        IDBDatabase, IDBDatabaseClass,
        js_fun_createObjectStore_DOMString_nullable_IDBObjectStoreParameters_IDBObjectStore,
        js_fun_deleteObjectStore_DOMString_undefined,
        js_fun_transaction_Union_DOMString_sequence_DOMString_EndUnion_nullable_IDBTransactionMode_IDBTransaction,
        js_fun_close__undefined,
        js_fun_createMutableFile_DOMString_nullable_DOMString_IDBRequest,
        js_get_name, js_get_version, js_get_objectStoreNames,
        js_get_onabort, js_set_onabort, js_get_onclose, js_set_onclose,
        js_get_onerror, js_set_onerror, js_get_onversionchange,
        js_set_onversionchange, js_get_storage
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMStringList.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.IDBDatabase.Core
import GHC.Wasm.Web.Generated.IDBObjectStore.Core
import GHC.Wasm.Web.Generated.IDBObjectStoreParameters.Core
import GHC.Wasm.Web.Generated.IDBRequest.Core
import GHC.Wasm.Web.Generated.IDBTransaction.Core
import GHC.Wasm.Web.Generated.IDBTransactionMode.Core
import GHC.Wasm.Web.Generated.StorageType.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.createObjectStore($2,$3)" js_fun_createObjectStore_DOMString_nullable_IDBObjectStoreParameters_IDBObjectStore
  :: IDBDatabase
     -> (DOMString
         -> (Nullable IDBObjectStoreParametersClass -> (IO IDBObjectStore)))
foreign import javascript unsafe "$1.deleteObjectStore($2)" js_fun_deleteObjectStore_DOMString_undefined
  :: IDBDatabase -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.transaction($2,$3)" js_fun_transaction_Union_DOMString_sequence_DOMString_EndUnion_nullable_IDBTransactionMode_IDBTransaction
  :: IDBDatabase
     -> (JSObject (UnionClass '[DOMStringClass,
                                SequenceClass DOMStringClass])
         -> (Nullable IDBTransactionModeClass -> (IO IDBTransaction)))
foreign import javascript unsafe "$1.close()" js_fun_close__undefined
  :: IDBDatabase -> (IO ())
foreign import javascript unsafe "$1.createMutableFile($2,$3)" js_fun_createMutableFile_DOMString_nullable_DOMString_IDBRequest
  :: IDBDatabase
     -> (DOMString -> (Nullable DOMStringClass -> (IO IDBRequest)))
foreign import javascript unsafe "$1.name" js_get_name
  :: IDBDatabase -> (IO DOMString)
foreign import javascript unsafe "$1.version" js_get_version
  :: IDBDatabase -> (IO Word64)
foreign import javascript unsafe "$1.objectStoreNames" js_get_objectStoreNames
  :: IDBDatabase -> (IO DOMStringList)
foreign import javascript unsafe "$1.onabort" js_get_onabort
  :: IDBDatabase -> (IO EventHandler)
foreign import javascript unsafe "$1.onabort = $2" js_set_onabort
  :: IDBDatabase -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onclose" js_get_onclose
  :: IDBDatabase -> (IO EventHandler)
foreign import javascript unsafe "$1.onclose = $2" js_set_onclose
  :: IDBDatabase -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onerror" js_get_onerror
  :: IDBDatabase -> (IO EventHandler)
foreign import javascript unsafe "$1.onerror = $2" js_set_onerror
  :: IDBDatabase -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onversionchange" js_get_onversionchange
  :: IDBDatabase -> (IO EventHandler)
foreign import javascript unsafe "$1.onversionchange = $2" js_set_onversionchange
  :: IDBDatabase -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.storage" js_get_storage
  :: IDBDatabase -> (IO StorageType)
