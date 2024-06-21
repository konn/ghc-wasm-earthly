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
module GHC.Wasm.Web.Generated.SVGStringList (
        SVGStringList, SVGStringListClass, js_fun_clear__undefined,
        js_fun_initialize_DOMString_DOMString,
        js_fun_getItem_long_DOMString,
        js_fun_insertItemBefore_DOMString_long_DOMString,
        js_fun_replaceItem_DOMString_long_DOMString,
        js_fun_removeItem_long_DOMString,
        js_fun_appendItem_DOMString_DOMString, js_get_length,
        js_get_numberOfItems
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.SVGStringList.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.clear()" js_fun_clear__undefined
  :: SVGStringList -> (IO ())
foreign import javascript unsafe "$1.initialize($2)" js_fun_initialize_DOMString_DOMString
  :: SVGStringList -> (DOMString -> (IO DOMString))
foreign import javascript unsafe "$1.getItem($2)" js_fun_getItem_long_DOMString
  :: SVGStringList -> (Word32 -> (IO DOMString))
foreign import javascript unsafe "$1.insertItemBefore($2,$3)" js_fun_insertItemBefore_DOMString_long_DOMString
  :: SVGStringList -> (DOMString -> (Word32 -> (IO DOMString)))
foreign import javascript unsafe "$1.replaceItem($2,$3)" js_fun_replaceItem_DOMString_long_DOMString
  :: SVGStringList -> (DOMString -> (Word32 -> (IO DOMString)))
foreign import javascript unsafe "$1.removeItem($2)" js_fun_removeItem_long_DOMString
  :: SVGStringList -> (Word32 -> (IO DOMString))
foreign import javascript unsafe "$1.appendItem($2)" js_fun_appendItem_DOMString_DOMString
  :: SVGStringList -> (DOMString -> (IO DOMString))
foreign import javascript unsafe "$1.length" js_get_length
  :: SVGStringList -> (IO Word32)
foreign import javascript unsafe "$1.numberOfItems" js_get_numberOfItems
  :: SVGStringList -> (IO Word32)
