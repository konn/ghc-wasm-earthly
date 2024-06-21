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
module GHC.Wasm.Web.Generated.IDBOpenDBRequest (
        IDBOpenDBRequest, IDBOpenDBRequestClass, js_get_onblocked,
        js_set_onblocked, js_get_onupgradeneeded, js_set_onupgradeneeded
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.IDBOpenDBRequest.Core
import GHC.Wasm.Web.Generated.IDBRequest.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.onblocked" js_get_onblocked
  :: IDBOpenDBRequest -> (IO EventHandler)
foreign import javascript unsafe "$1.onblocked = $2" js_set_onblocked
  :: IDBOpenDBRequest -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onupgradeneeded" js_get_onupgradeneeded
  :: IDBOpenDBRequest -> (IO EventHandler)
foreign import javascript unsafe "$1.onupgradeneeded = $2" js_set_onupgradeneeded
  :: IDBOpenDBRequest -> (EventHandler -> (IO ()))
