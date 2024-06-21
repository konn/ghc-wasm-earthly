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
module GHC.Wasm.Web.Generated.U2FRegisterCallback.Core (
        U2FRegisterCallbackClass, U2FRegisterCallback,
        js_mk_callback_U2FRegisterCallback_impure
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.RegisterResponse.Core
import GHC.Wasm.Web.Types
type data U2FRegisterCallbackClass :: Prototype
type instance SuperclassOf U2FRegisterCallbackClass = 'Nothing
type U2FRegisterCallback = JSObject U2FRegisterCallbackClass
foreign import javascript unsafe "wrapper" js_mk_callback_U2FRegisterCallback_impure
  :: (RegisterResponse -> (IO ())) -> U2FRegisterCallback
