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
module GHC.Wasm.Web.Generated.IDBFactory (
        IDBFactory, IDBFactoryClass,
        js_fun_open_DOMString_longlong_IDBOpenDBRequest,
        js_fun_open_DOMString_nullable_IDBOpenDBOptions_IDBOpenDBRequest,
        js_fun_deleteDatabase_DOMString_nullable_IDBOpenDBOptions_IDBOpenDBRequest,
        js_fun_cmp_any_any_short,
        js_fun_openForPrincipal_Principal_DOMString_longlong_IDBOpenDBRequest,
        js_fun_openForPrincipal_Principal_DOMString_nullable_IDBOpenDBOptions_IDBOpenDBRequest,
        js_fun_deleteForPrincipal_Principal_DOMString_nullable_IDBOpenDBOptions_IDBOpenDBRequest
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.IDBFactory.Core
import GHC.Wasm.Web.Generated.IDBOpenDBOptions.Core
import GHC.Wasm.Web.Generated.IDBOpenDBRequest.Core
import GHC.Wasm.Web.Generated.Principal.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.open($2,$3)" js_fun_open_DOMString_longlong_IDBOpenDBRequest
  :: IDBFactory -> (DOMString -> (Word64 -> (IO IDBOpenDBRequest)))
foreign import javascript unsafe "$1.open($2,$3)" js_fun_open_DOMString_nullable_IDBOpenDBOptions_IDBOpenDBRequest
  :: IDBFactory
     -> (DOMString
         -> (Nullable IDBOpenDBOptionsClass -> (IO IDBOpenDBRequest)))
foreign import javascript unsafe "$1.deleteDatabase($2,$3)" js_fun_deleteDatabase_DOMString_nullable_IDBOpenDBOptions_IDBOpenDBRequest
  :: IDBFactory
     -> (DOMString
         -> (Nullable IDBOpenDBOptionsClass -> (IO IDBOpenDBRequest)))
foreign import javascript unsafe "$1.cmp($2,$3)" js_fun_cmp_any_any_short
  :: IDBFactory -> (JSAny -> (JSAny -> (IO Int16)))
foreign import javascript unsafe "$1.openForPrincipal($2,$3,$4)" js_fun_openForPrincipal_Principal_DOMString_longlong_IDBOpenDBRequest
  :: IDBFactory
     -> (Principal -> (DOMString -> (Word64 -> (IO IDBOpenDBRequest))))
foreign import javascript unsafe "$1.openForPrincipal($2,$3,$4)" js_fun_openForPrincipal_Principal_DOMString_nullable_IDBOpenDBOptions_IDBOpenDBRequest
  :: IDBFactory
     -> (Principal
         -> (DOMString
             -> (Nullable IDBOpenDBOptionsClass -> (IO IDBOpenDBRequest))))
foreign import javascript unsafe "$1.deleteForPrincipal($2,$3,$4)" js_fun_deleteForPrincipal_Principal_DOMString_nullable_IDBOpenDBOptions_IDBOpenDBRequest
  :: IDBFactory
     -> (Principal
         -> (DOMString
             -> (Nullable IDBOpenDBOptionsClass -> (IO IDBOpenDBRequest))))
