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
module GHC.Wasm.Web.Generated.MediaList (
        MediaList, MediaListClass, js_fun_deleteMedium_DOMString_undefined,
        js_fun_appendMedium_DOMString_undefined, js_get_mediaText,
        js_set_mediaText, js_get_length
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MediaList.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.deleteMedium($2)" js_fun_deleteMedium_DOMString_undefined
  :: MediaList -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.appendMedium($2)" js_fun_appendMedium_DOMString_undefined
  :: MediaList -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.mediaText" js_get_mediaText
  :: MediaList -> (IO DOMString)
foreign import javascript unsafe "$1.mediaText = $2" js_set_mediaText
  :: MediaList -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.length" js_get_length
  :: MediaList -> (IO Word32)
