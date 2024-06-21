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
module GHC.Wasm.Web.Generated.Clipboard (
        Clipboard, ClipboardClass, js_fun_read__Promise_ClipboardItems,
        js_fun_readText__Promise_DOMString,
        js_fun_write_ClipboardItems_Promise_undefined,
        js_fun_writeText_DOMString_Promise_undefined
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Clipboard.Core
import GHC.Wasm.Web.Generated.ClipboardItems.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.read()" js_fun_read__Promise_ClipboardItems
  :: Clipboard -> (IO (Promise ClipboardItemsClass))
foreign import javascript safe "$1.readText()" js_fun_readText__Promise_DOMString
  :: Clipboard -> (IO (Promise DOMStringClass))
foreign import javascript safe "$1.write($2)" js_fun_write_ClipboardItems_Promise_undefined
  :: Clipboard -> (ClipboardItems -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.writeText($2)" js_fun_writeText_DOMString_Promise_undefined
  :: Clipboard -> (DOMString -> (IO (Promise UndefinedClass)))
