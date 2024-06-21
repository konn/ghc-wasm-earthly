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
module GHC.Wasm.Web.Generated.ClipboardItem (
        ClipboardItem, ClipboardItemClass, js_cons_ClipboardItem,
        js_fun_getType_DOMString_Promise_Blob, js_get_presentationStyle,
        js_get_lastModified, js_get_delayed, js_get_types
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Blob.Core
import GHC.Wasm.Web.Generated.ClipboardItem.Core
import GHC.Wasm.Web.Generated.ClipboardItemData.Core
import GHC.Wasm.Web.Generated.ClipboardItemDelayedCallback.Core
import GHC.Wasm.Web.Generated.ClipboardItemOptions.Core
import GHC.Wasm.Web.Generated.PresentationStyle.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new ClipboardItem($1,$2)" js_cons_ClipboardItem
  :: JSRecord DOMStringClass ClipboardItemDataClass
     -> (Nullable ClipboardItemOptionsClass -> (IO ClipboardItem))
foreign import javascript safe "$1.getType($2)" js_fun_getType_DOMString_Promise_Blob
  :: ClipboardItem -> (DOMString -> (IO (Promise BlobClass)))
foreign import javascript unsafe "$1.presentationStyle" js_get_presentationStyle
  :: ClipboardItem -> (IO PresentationStyle)
foreign import javascript unsafe "$1.lastModified" js_get_lastModified
  :: ClipboardItem -> (IO Int64)
foreign import javascript unsafe "$1.delayed" js_get_delayed
  :: ClipboardItem -> (IO Bool)
foreign import javascript unsafe "$1.types" js_get_types
  :: ClipboardItem -> (IO (FrozenArray DOMStringClass))
