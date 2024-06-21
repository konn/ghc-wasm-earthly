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
module GHC.Wasm.Web.Generated.ClipboardItemDelayedCallback.Core (
        ClipboardItemDelayedCallbackClass, ClipboardItemDelayedCallback,
        js_mk_callback_ClipboardItemDelayedCallback_impure
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ClipboardItemData.Core
import GHC.Wasm.Web.Types
type data ClipboardItemDelayedCallbackClass :: Prototype
type instance SuperclassOf ClipboardItemDelayedCallbackClass = 'Nothing
type ClipboardItemDelayedCallback =
    JSObject ClipboardItemDelayedCallbackClass
foreign import javascript unsafe "wrapper" js_mk_callback_ClipboardItemDelayedCallback_impure
  :: IO ClipboardItemData -> ClipboardItemDelayedCallback
