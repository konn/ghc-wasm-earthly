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
module GHC.Wasm.Web.Generated.PositionErrorCallback.Core (
        PositionErrorCallbackClass, PositionErrorCallback,
        js_mk_callback_PositionErrorCallback_impure
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.PositionError.Core
import GHC.Wasm.Web.Types
type data PositionErrorCallbackClass :: Prototype
type instance SuperclassOf PositionErrorCallbackClass = 'Nothing
type PositionErrorCallback = JSObject PositionErrorCallbackClass
foreign import javascript unsafe "wrapper" js_mk_callback_PositionErrorCallback_impure
  :: (PositionError -> (IO ())) -> PositionErrorCallback
