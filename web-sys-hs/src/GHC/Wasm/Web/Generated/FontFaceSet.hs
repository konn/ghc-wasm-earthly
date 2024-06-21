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
module GHC.Wasm.Web.Generated.FontFaceSet (
        FontFaceSet, FontFaceSetClass, js_fun_add_FontFace_undefined,
        js_fun_has_FontFace_boolean, js_fun_delete_FontFace_boolean,
        js_fun_clear__undefined, js_fun_entries__FontFaceSetIterator,
        js_fun_values__FontFaceSetIterator,
        js_fun_forEach_FontFaceSetForEachCallback_nullable_any_undefined,
        js_fun_load_DOMString_nullable_DOMString_Promise_sequence_FontFace,
        js_fun_check_DOMString_nullable_DOMString_boolean, js_get_size,
        js_get_onloading, js_set_onloading, js_get_onloadingdone,
        js_set_onloadingdone, js_get_onloadingerror, js_set_onloadingerror,
        js_get_ready, js_get_status
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.FontFace.Core
import GHC.Wasm.Web.Generated.FontFaceSet.Core
import GHC.Wasm.Web.Generated.FontFaceSetForEachCallback.Core
import GHC.Wasm.Web.Generated.FontFaceSetIterator.Core
import GHC.Wasm.Web.Generated.FontFaceSetLoadStatus.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.add($2)" js_fun_add_FontFace_undefined
  :: FontFaceSet -> (FontFace -> (IO ()))
foreign import javascript unsafe "$1.has($2)" js_fun_has_FontFace_boolean
  :: FontFaceSet -> (FontFace -> (IO Bool))
foreign import javascript unsafe "$1.delete($2)" js_fun_delete_FontFace_boolean
  :: FontFaceSet -> (FontFace -> (IO Bool))
foreign import javascript unsafe "$1.clear()" js_fun_clear__undefined
  :: FontFaceSet -> (IO ())
foreign import javascript unsafe "$1.entries()" js_fun_entries__FontFaceSetIterator
  :: FontFaceSet -> (IO FontFaceSetIterator)
foreign import javascript unsafe "$1.values()" js_fun_values__FontFaceSetIterator
  :: FontFaceSet -> (IO FontFaceSetIterator)
foreign import javascript unsafe "$1.forEach($2,$3)" js_fun_forEach_FontFaceSetForEachCallback_nullable_any_undefined
  :: FontFaceSet
     -> (FontFaceSetForEachCallback -> (Nullable AnyClass -> (IO ())))
foreign import javascript safe "$1.load($2,$3)" js_fun_load_DOMString_nullable_DOMString_Promise_sequence_FontFace
  :: FontFaceSet
     -> (DOMString
         -> (Nullable DOMStringClass
             -> (IO (Promise (SequenceClass FontFaceClass)))))
foreign import javascript unsafe "$1.check($2,$3)" js_fun_check_DOMString_nullable_DOMString_boolean
  :: FontFaceSet
     -> (DOMString -> (Nullable DOMStringClass -> (IO Bool)))
foreign import javascript unsafe "$1.size" js_get_size
  :: FontFaceSet -> (IO Word32)
foreign import javascript unsafe "$1.onloading" js_get_onloading
  :: FontFaceSet -> (IO EventHandler)
foreign import javascript unsafe "$1.onloading = $2" js_set_onloading
  :: FontFaceSet -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadingdone" js_get_onloadingdone
  :: FontFaceSet -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadingdone = $2" js_set_onloadingdone
  :: FontFaceSet -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadingerror" js_get_onloadingerror
  :: FontFaceSet -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadingerror = $2" js_set_onloadingerror
  :: FontFaceSet -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ready" js_get_ready
  :: FontFaceSet -> (IO (Promise UndefinedClass))
foreign import javascript unsafe "$1.status" js_get_status
  :: FontFaceSet -> (IO FontFaceSetLoadStatus)
