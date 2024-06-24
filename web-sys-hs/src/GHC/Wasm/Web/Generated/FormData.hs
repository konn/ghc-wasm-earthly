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
module GHC.Wasm.Web.Generated.FormData (
        FormData, FormDataClass, js_cons_FormData,
        js_fun_append_USVString_Blob_nullable_USVString_undefined,
        js_fun_append_USVString_USVString_undefined,
        js_fun_delete_USVString_undefined,
        js_fun_get_USVString_nullable_FormDataEntryValue,
        js_fun_getAll_USVString_sequence_FormDataEntryValue,
        js_fun_has_USVString_boolean,
        js_fun_set_USVString_Blob_nullable_USVString_undefined,
        js_fun_set_USVString_USVString_undefined,
        js_iter_FormData_USVString_FormDataEntryValue
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Blob.Core
import GHC.Wasm.Web.Generated.FormData.Core
import GHC.Wasm.Web.Generated.FormDataEntryValue.Core
import GHC.Wasm.Web.Generated.HTMLFormElement.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new FormData($1)" js_cons_FormData
  :: Nullable HTMLFormElementClass -> (IO FormData)
foreign import javascript unsafe "$1.append($2,$3,$4)" js_fun_append_USVString_Blob_nullable_USVString_undefined
  :: FormData
     -> (USVString -> (Blob -> (Nullable USVStringClass -> (IO ()))))
foreign import javascript unsafe "$1.append($2,$3)" js_fun_append_USVString_USVString_undefined
  :: FormData -> (USVString -> (USVString -> (IO ())))
foreign import javascript unsafe "$1.delete($2)" js_fun_delete_USVString_undefined
  :: FormData -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.get($2)" js_fun_get_USVString_nullable_FormDataEntryValue
  :: FormData
     -> (USVString -> (IO (Nullable FormDataEntryValueClass)))
foreign import javascript unsafe "$1.getAll($2)" js_fun_getAll_USVString_sequence_FormDataEntryValue
  :: FormData
     -> (USVString -> (IO (Sequence FormDataEntryValueClass)))
foreign import javascript unsafe "$1.has($2)" js_fun_has_USVString_boolean
  :: FormData -> (USVString -> (IO Bool))
foreign import javascript unsafe "$1.set($2,$3,$4)" js_fun_set_USVString_Blob_nullable_USVString_undefined
  :: FormData
     -> (USVString -> (Blob -> (Nullable USVStringClass -> (IO ()))))
foreign import javascript unsafe "$1.set($2,$3)" js_fun_set_USVString_USVString_undefined
  :: FormData -> (USVString -> (USVString -> (IO ())))
js_iter_FormData_USVString_FormDataEntryValue ::
  PairIterable USVStringClass FormDataEntryValueClass -> FormData
js_iter_FormData_USVString_FormDataEntryValue = unsafeCast
