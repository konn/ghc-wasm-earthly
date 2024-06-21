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
module GHC.Wasm.Web.Generated.FrameRequestCallback.Core (
        FrameRequestCallbackClass, FrameRequestCallback,
        js_mk_callback_FrameRequestCallback_impure
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMHighResTimeStamp.Core
import GHC.Wasm.Web.Types
type data FrameRequestCallbackClass :: Prototype
type instance SuperclassOf FrameRequestCallbackClass = 'Nothing
type FrameRequestCallback = JSObject FrameRequestCallbackClass
foreign import javascript unsafe "wrapper" js_mk_callback_FrameRequestCallback_impure
  :: (DOMHighResTimeStamp -> (IO ())) -> FrameRequestCallback
