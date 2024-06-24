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
module GHC.Wasm.Web.Generated.URLSearchParams (
        URLSearchParams, URLSearchParamsClass, js_cons_URLSearchParams,
        js_fun_append_USVString_USVString_undefined,
        js_fun_delete_USVString_undefined,
        js_fun_get_USVString_nullable_USVString,
        js_fun_getAll_USVString_sequence_USVString,
        js_fun_has_USVString_boolean,
        js_fun_set_USVString_USVString_undefined, js_fun_sort__undefined,
        js_iter_URLSearchParams_USVString_USVString
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.URLSearchParams.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new URLSearchParams($1)" js_cons_URLSearchParams
  :: Nullable (UnionClass '[SequenceClass (SequenceClass USVStringClass),
                            JSRecordClass USVStringClass USVStringClass,
                            USVStringClass])
     -> (IO URLSearchParams)
foreign import javascript unsafe "$1.append($2,$3)" js_fun_append_USVString_USVString_undefined
  :: URLSearchParams -> (USVString -> (USVString -> (IO ())))
foreign import javascript unsafe "$1.delete($2)" js_fun_delete_USVString_undefined
  :: URLSearchParams -> (USVString -> (IO ()))
foreign import javascript unsafe "$1.get($2)" js_fun_get_USVString_nullable_USVString
  :: URLSearchParams -> (USVString -> (IO (Nullable USVStringClass)))
foreign import javascript unsafe "$1.getAll($2)" js_fun_getAll_USVString_sequence_USVString
  :: URLSearchParams -> (USVString -> (IO (Sequence USVStringClass)))
foreign import javascript unsafe "$1.has($2)" js_fun_has_USVString_boolean
  :: URLSearchParams -> (USVString -> (IO Bool))
foreign import javascript unsafe "$1.set($2,$3)" js_fun_set_USVString_USVString_undefined
  :: URLSearchParams -> (USVString -> (USVString -> (IO ())))
foreign import javascript unsafe "$1.sort()" js_fun_sort__undefined
  :: URLSearchParams -> (IO ())
js_iter_URLSearchParams_USVString_USVString ::
  URLSearchParams -> (PairIterable USVStringClass USVStringClass)
js_iter_URLSearchParams_USVString_USVString = unsafeCast
