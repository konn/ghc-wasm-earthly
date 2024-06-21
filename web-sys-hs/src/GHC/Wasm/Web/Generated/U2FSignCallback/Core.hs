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
module GHC.Wasm.Web.Generated.U2FSignCallback.Core (
        U2FSignCallbackClass, U2FSignCallback,
        js_mk_callback_U2FSignCallback_impure
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.SignResponse.Core
import GHC.Wasm.Web.Types
type data U2FSignCallbackClass :: Prototype
type instance SuperclassOf U2FSignCallbackClass = 'Nothing
type U2FSignCallback = JSObject U2FSignCallbackClass
foreign import javascript unsafe "wrapper" js_mk_callback_U2FSignCallback_impure
  :: (SignResponse -> (IO ())) -> U2FSignCallback
