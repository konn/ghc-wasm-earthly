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
module GHC.Wasm.Web.Generated.CharacterData (
        CharacterData, CharacterDataClass,
        js_fun_substringData_long_long_DOMString,
        js_fun_appendData_DOMString_undefined,
        js_fun_insertData_long_DOMString_undefined,
        js_fun_deleteData_long_long_undefined,
        js_fun_replaceData_long_long_DOMString_undefined,
        js_fun_before_Union_Node_DOMString_EndUnion_undefined,
        js_fun_after_Union_Node_DOMString_EndUnion_undefined,
        js_fun_replaceWith_Union_Node_DOMString_EndUnion_undefined,
        js_fun_remove__undefined, js_get_data, js_set_data, js_get_length,
        js_get_previousElementSibling, js_get_nextElementSibling
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.CharacterData.Core
import GHC.Wasm.Web.Generated.Element.Core
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.substringData($2,$3)" js_fun_substringData_long_long_DOMString
  :: CharacterData -> (Word32 -> (Word32 -> (IO DOMString)))
foreign import javascript unsafe "$1.appendData($2)" js_fun_appendData_DOMString_undefined
  :: CharacterData -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.insertData($2,$3)" js_fun_insertData_long_DOMString_undefined
  :: CharacterData -> (Word32 -> (DOMString -> (IO ())))
foreign import javascript unsafe "$1.deleteData($2,$3)" js_fun_deleteData_long_long_undefined
  :: CharacterData -> (Word32 -> (Word32 -> (IO ())))
foreign import javascript unsafe "$1.replaceData($2,$3,$4)" js_fun_replaceData_long_long_DOMString_undefined
  :: CharacterData -> (Word32 -> (Word32 -> (DOMString -> (IO ()))))
foreign import javascript unsafe "$1.before(... $2)" js_fun_before_Union_Node_DOMString_EndUnion_undefined
  :: CharacterData
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.after(... $2)" js_fun_after_Union_Node_DOMString_EndUnion_undefined
  :: CharacterData
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.replaceWith(... $2)" js_fun_replaceWith_Union_Node_DOMString_EndUnion_undefined
  :: CharacterData
     -> (FrozenArray (UnionClass '[NodeClass, DOMStringClass])
         -> (IO ()))
foreign import javascript unsafe "$1.remove()" js_fun_remove__undefined
  :: CharacterData -> (IO ())
foreign import javascript unsafe "$1.data" js_get_data
  :: CharacterData -> (IO DOMString)
foreign import javascript unsafe "$1.data = $2" js_set_data
  :: CharacterData -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.length" js_get_length
  :: CharacterData -> (IO Word32)
foreign import javascript unsafe "$1.previousElementSibling" js_get_previousElementSibling
  :: CharacterData -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.nextElementSibling" js_get_nextElementSibling
  :: CharacterData -> (IO (Nullable ElementClass))
