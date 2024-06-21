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
module GHC.Wasm.Web.Generated.IdleRequestCallback.Core (
        IdleRequestCallbackClass, IdleRequestCallback,
        js_mk_callback_IdleRequestCallback_impure
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.IdleDeadline.Core
import GHC.Wasm.Web.Types
type data IdleRequestCallbackClass :: Prototype
type instance SuperclassOf IdleRequestCallbackClass = 'Nothing
type IdleRequestCallback = JSObject IdleRequestCallbackClass
foreign import javascript unsafe "wrapper" js_mk_callback_IdleRequestCallback_impure
  :: (IdleDeadline -> (IO ())) -> IdleRequestCallback
