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
module GHC.Wasm.Web.Generated.IDBRequest (
        IDBRequest, IDBRequestClass, js_get_result, js_get_error,
        js_get_source, js_get_transaction, js_get_readyState,
        js_get_onsuccess, js_set_onsuccess, js_get_onerror, js_set_onerror
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMException.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.IDBCursor.Core
import GHC.Wasm.Web.Generated.IDBIndex.Core
import GHC.Wasm.Web.Generated.IDBObjectStore.Core
import GHC.Wasm.Web.Generated.IDBRequest.Core
import GHC.Wasm.Web.Generated.IDBRequestReadyState.Core
import GHC.Wasm.Web.Generated.IDBTransaction.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.result" js_get_result
  :: IDBRequest -> (IO JSAny)
foreign import javascript unsafe "$1.error" js_get_error
  :: IDBRequest -> (IO (Nullable DOMExceptionClass))
foreign import javascript unsafe "$1.source" js_get_source
  :: IDBRequest
     -> (IO (Nullable (UnionClass '[IDBObjectStoreClass,
                                    IDBIndexClass,
                                    IDBCursorClass])))
foreign import javascript unsafe "$1.transaction" js_get_transaction
  :: IDBRequest -> (IO (Nullable IDBTransactionClass))
foreign import javascript unsafe "$1.readyState" js_get_readyState
  :: IDBRequest -> (IO IDBRequestReadyState)
foreign import javascript unsafe "$1.onsuccess" js_get_onsuccess
  :: IDBRequest -> (IO EventHandler)
foreign import javascript unsafe "$1.onsuccess = $2" js_set_onsuccess
  :: IDBRequest -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onerror" js_get_onerror
  :: IDBRequest -> (IO EventHandler)
foreign import javascript unsafe "$1.onerror = $2" js_set_onerror
  :: IDBRequest -> (EventHandler -> (IO ()))
