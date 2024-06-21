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
module GHC.Wasm.Web.Generated.PerformanceEntry (
        PerformanceEntry, PerformanceEntryClass, js_fun_toJSON__object,
        js_get_name, js_get_entryType, js_get_startTime, js_get_duration
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMHighResTimeStamp.Core
import GHC.Wasm.Web.Generated.PerformanceEntry.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.toJSON()" js_fun_toJSON__object
  :: PerformanceEntry -> (IO JSAny)
foreign import javascript unsafe "$1.name" js_get_name
  :: PerformanceEntry -> (IO DOMString)
foreign import javascript unsafe "$1.entryType" js_get_entryType
  :: PerformanceEntry -> (IO DOMString)
foreign import javascript unsafe "$1.startTime" js_get_startTime
  :: PerformanceEntry -> (IO DOMHighResTimeStamp)
foreign import javascript unsafe "$1.duration" js_get_duration
  :: PerformanceEntry -> (IO DOMHighResTimeStamp)
