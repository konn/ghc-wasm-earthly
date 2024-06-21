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
module GHC.Wasm.Web.Generated.ObserverCallback.Core (
        ObserverCallbackClass, ObserverCallback,
        js_mk_callback_ObserverCallback
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.FetchObserver.Core
import GHC.Wasm.Web.Types
type data ObserverCallbackClass :: Prototype
type instance SuperclassOf ObserverCallbackClass = 'Nothing
type ObserverCallback = JSObject ObserverCallbackClass
foreign import javascript unsafe "wrapper" js_mk_callback_ObserverCallback
  :: (FetchObserver -> (IO ())) -> ObserverCallback
