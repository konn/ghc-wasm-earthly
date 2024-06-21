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
module GHC.Wasm.Web.Generated.IDBIndex (
        IDBIndex, IDBIndexClass,
        js_fun_openCursor_nullable_any_nullable_IDBCursorDirection_IDBRequest,
        js_fun_openKeyCursor_nullable_any_nullable_IDBCursorDirection_IDBRequest,
        js_fun_get_any_IDBRequest, js_fun_getKey_any_IDBRequest,
        js_fun_count_nullable_any_IDBRequest,
        js_fun_getAll_nullable_any_nullable_long_IDBRequest,
        js_fun_getAllKeys_nullable_any_nullable_long_IDBRequest,
        js_get_name, js_set_name, js_get_objectStore, js_get_keyPath,
        js_get_multiEntry, js_get_unique, js_get_locale,
        js_get_isAutoLocale
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.IDBCursorDirection.Core
import GHC.Wasm.Web.Generated.IDBIndex.Core
import GHC.Wasm.Web.Generated.IDBObjectStore.Core
import GHC.Wasm.Web.Generated.IDBRequest.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.openCursor($2,$3)" js_fun_openCursor_nullable_any_nullable_IDBCursorDirection_IDBRequest
  :: IDBIndex
     -> (Nullable AnyClass
         -> (Nullable IDBCursorDirectionClass -> (IO IDBRequest)))
foreign import javascript unsafe "$1.openKeyCursor($2,$3)" js_fun_openKeyCursor_nullable_any_nullable_IDBCursorDirection_IDBRequest
  :: IDBIndex
     -> (Nullable AnyClass
         -> (Nullable IDBCursorDirectionClass -> (IO IDBRequest)))
foreign import javascript unsafe "$1.get($2)" js_fun_get_any_IDBRequest
  :: IDBIndex -> (JSAny -> (IO IDBRequest))
foreign import javascript unsafe "$1.getKey($2)" js_fun_getKey_any_IDBRequest
  :: IDBIndex -> (JSAny -> (IO IDBRequest))
foreign import javascript unsafe "$1.count($2)" js_fun_count_nullable_any_IDBRequest
  :: IDBIndex -> (Nullable AnyClass -> (IO IDBRequest))
foreign import javascript unsafe "$1.getAll($2,$3)" js_fun_getAll_nullable_any_nullable_long_IDBRequest
  :: IDBIndex
     -> (Nullable AnyClass
         -> (Nullable (JSPrimClass Word32) -> (IO IDBRequest)))
foreign import javascript unsafe "$1.getAllKeys($2,$3)" js_fun_getAllKeys_nullable_any_nullable_long_IDBRequest
  :: IDBIndex
     -> (Nullable AnyClass
         -> (Nullable (JSPrimClass Word32) -> (IO IDBRequest)))
foreign import javascript unsafe "$1.name" js_get_name
  :: IDBIndex -> (IO DOMString)
foreign import javascript unsafe "$1.name = $2" js_set_name
  :: IDBIndex -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.objectStore" js_get_objectStore
  :: IDBIndex -> (IO IDBObjectStore)
foreign import javascript unsafe "$1.keyPath" js_get_keyPath
  :: IDBIndex -> (IO JSAny)
foreign import javascript unsafe "$1.multiEntry" js_get_multiEntry
  :: IDBIndex -> (IO Bool)
foreign import javascript unsafe "$1.unique" js_get_unique
  :: IDBIndex -> (IO Bool)
foreign import javascript unsafe "$1.locale" js_get_locale
  :: IDBIndex -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.isAutoLocale" js_get_isAutoLocale
  :: IDBIndex -> (IO Bool)
