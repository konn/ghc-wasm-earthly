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
module GHC.Wasm.Web.Generated.Performance (
        Performance, PerformanceClass, js_fun_now__DOMHighResTimeStamp,
        js_fun_measureUserAgentSpecificMemory__Promise_MemoryMeasurement,
        js_fun_toJSON__object, js_fun_getEntries__PerformanceEntryList,
        js_fun_getEntriesByType_DOMString_PerformanceEntryList,
        js_fun_getEntriesByName_DOMString_nullable_DOMString_PerformanceEntryList,
        js_fun_clearResourceTimings__undefined,
        js_fun_setResourceTimingBufferSize_long_undefined,
        js_fun_mark_DOMString_undefined,
        js_fun_clearMarks_nullable_DOMString_undefined,
        js_fun_measure_DOMString_nullable_DOMString_nullable_DOMString_undefined,
        js_fun_clearMeasures_nullable_DOMString_undefined,
        js_get_timeOrigin, js_get_timing, js_get_navigation,
        js_get_onresourcetimingbufferfull,
        js_set_onresourcetimingbufferfull
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMHighResTimeStamp.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.MemoryMeasurement.Core
import GHC.Wasm.Web.Generated.Performance.Core
import GHC.Wasm.Web.Generated.PerformanceEntryList.Core
import GHC.Wasm.Web.Generated.PerformanceNavigation.Core
import GHC.Wasm.Web.Generated.PerformanceTiming.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.now()" js_fun_now__DOMHighResTimeStamp
  :: Performance -> (IO DOMHighResTimeStamp)
foreign import javascript safe "$1.measureUserAgentSpecificMemory()" js_fun_measureUserAgentSpecificMemory__Promise_MemoryMeasurement
  :: Performance -> (IO (Promise MemoryMeasurementClass))
foreign import javascript unsafe "$1.toJSON()" js_fun_toJSON__object
  :: Performance -> (IO JSAny)
foreign import javascript unsafe "$1.getEntries()" js_fun_getEntries__PerformanceEntryList
  :: Performance -> (IO PerformanceEntryList)
foreign import javascript unsafe "$1.getEntriesByType($2)" js_fun_getEntriesByType_DOMString_PerformanceEntryList
  :: Performance -> (DOMString -> (IO PerformanceEntryList))
foreign import javascript unsafe "$1.getEntriesByName($2,$3)" js_fun_getEntriesByName_DOMString_nullable_DOMString_PerformanceEntryList
  :: Performance
     -> (DOMString
         -> (Nullable DOMStringClass -> (IO PerformanceEntryList)))
foreign import javascript unsafe "$1.clearResourceTimings()" js_fun_clearResourceTimings__undefined
  :: Performance -> (IO ())
foreign import javascript unsafe "$1.setResourceTimingBufferSize($2)" js_fun_setResourceTimingBufferSize_long_undefined
  :: Performance -> (Word32 -> (IO ()))
foreign import javascript unsafe "$1.mark($2)" js_fun_mark_DOMString_undefined
  :: Performance -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.clearMarks($2)" js_fun_clearMarks_nullable_DOMString_undefined
  :: Performance -> (Nullable DOMStringClass -> (IO ()))
foreign import javascript unsafe "$1.measure($2,$3,$4)" js_fun_measure_DOMString_nullable_DOMString_nullable_DOMString_undefined
  :: Performance
     -> (DOMString
         -> (Nullable DOMStringClass
             -> (Nullable DOMStringClass -> (IO ()))))
foreign import javascript unsafe "$1.clearMeasures($2)" js_fun_clearMeasures_nullable_DOMString_undefined
  :: Performance -> (Nullable DOMStringClass -> (IO ()))
foreign import javascript unsafe "$1.timeOrigin" js_get_timeOrigin
  :: Performance -> (IO DOMHighResTimeStamp)
foreign import javascript unsafe "$1.timing" js_get_timing
  :: Performance -> (IO PerformanceTiming)
foreign import javascript unsafe "$1.navigation" js_get_navigation
  :: Performance -> (IO PerformanceNavigation)
foreign import javascript unsafe "$1.onresourcetimingbufferfull" js_get_onresourcetimingbufferfull
  :: Performance -> (IO EventHandler)
foreign import javascript unsafe "$1.onresourcetimingbufferfull = $2" js_set_onresourcetimingbufferfull
  :: Performance -> (EventHandler -> (IO ()))
