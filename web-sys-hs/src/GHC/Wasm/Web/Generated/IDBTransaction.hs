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
module GHC.Wasm.Web.Generated.IDBTransaction (
        IDBTransaction, IDBTransactionClass,
        js_fun_objectStore_DOMString_IDBObjectStore,
        js_fun_commit__undefined, js_fun_abort__undefined, js_get_mode,
        js_get_db, js_get_error, js_get_onabort, js_set_onabort,
        js_get_oncomplete, js_set_oncomplete, js_get_onerror,
        js_set_onerror, js_get_objectStoreNames
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMException.Core
import GHC.Wasm.Web.Generated.DOMStringList.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.IDBDatabase.Core
import GHC.Wasm.Web.Generated.IDBObjectStore.Core
import GHC.Wasm.Web.Generated.IDBTransaction.Core
import GHC.Wasm.Web.Generated.IDBTransactionMode.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.objectStore($2)" js_fun_objectStore_DOMString_IDBObjectStore
  :: IDBTransaction -> (DOMString -> (IO IDBObjectStore))
foreign import javascript unsafe "$1.commit()" js_fun_commit__undefined
  :: IDBTransaction -> (IO ())
foreign import javascript unsafe "$1.abort()" js_fun_abort__undefined
  :: IDBTransaction -> (IO ())
foreign import javascript unsafe "$1.mode" js_get_mode
  :: IDBTransaction -> (IO IDBTransactionMode)
foreign import javascript unsafe "$1.db" js_get_db
  :: IDBTransaction -> (IO IDBDatabase)
foreign import javascript unsafe "$1.error" js_get_error
  :: IDBTransaction -> (IO (Nullable DOMExceptionClass))
foreign import javascript unsafe "$1.onabort" js_get_onabort
  :: IDBTransaction -> (IO EventHandler)
foreign import javascript unsafe "$1.onabort = $2" js_set_onabort
  :: IDBTransaction -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncomplete" js_get_oncomplete
  :: IDBTransaction -> (IO EventHandler)
foreign import javascript unsafe "$1.oncomplete = $2" js_set_oncomplete
  :: IDBTransaction -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onerror" js_get_onerror
  :: IDBTransaction -> (IO EventHandler)
foreign import javascript unsafe "$1.onerror = $2" js_set_onerror
  :: IDBTransaction -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.objectStoreNames" js_get_objectStoreNames
  :: IDBTransaction -> (IO DOMStringList)
