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
module GHC.Wasm.Web.Generated.PerformanceTiming (
        PerformanceTiming, PerformanceTimingClass, js_fun_toJSON__object,
        js_get_navigationStart, js_get_unloadEventStart,
        js_get_unloadEventEnd, js_get_redirectStart, js_get_redirectEnd,
        js_get_fetchStart, js_get_domainLookupStart,
        js_get_domainLookupEnd, js_get_connectStart, js_get_connectEnd,
        js_get_secureConnectionStart, js_get_requestStart,
        js_get_responseStart, js_get_responseEnd, js_get_domLoading,
        js_get_domInteractive, js_get_domContentLoadedEventStart,
        js_get_domContentLoadedEventEnd, js_get_domComplete,
        js_get_loadEventStart, js_get_loadEventEnd,
        js_get_timeToNonBlankPaint, js_get_timeToDOMContentFlushed
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.PerformanceTiming.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.toJSON()" js_fun_toJSON__object
  :: PerformanceTiming -> (IO JSAny)
foreign import javascript unsafe "$1.navigationStart" js_get_navigationStart
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.unloadEventStart" js_get_unloadEventStart
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.unloadEventEnd" js_get_unloadEventEnd
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.redirectStart" js_get_redirectStart
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.redirectEnd" js_get_redirectEnd
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.fetchStart" js_get_fetchStart
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.domainLookupStart" js_get_domainLookupStart
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.domainLookupEnd" js_get_domainLookupEnd
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.connectStart" js_get_connectStart
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.connectEnd" js_get_connectEnd
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.secureConnectionStart" js_get_secureConnectionStart
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.requestStart" js_get_requestStart
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.responseStart" js_get_responseStart
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.responseEnd" js_get_responseEnd
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.domLoading" js_get_domLoading
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.domInteractive" js_get_domInteractive
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.domContentLoadedEventStart" js_get_domContentLoadedEventStart
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.domContentLoadedEventEnd" js_get_domContentLoadedEventEnd
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.domComplete" js_get_domComplete
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.loadEventStart" js_get_loadEventStart
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.loadEventEnd" js_get_loadEventEnd
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.timeToNonBlankPaint" js_get_timeToNonBlankPaint
  :: PerformanceTiming -> (IO Word64)
foreign import javascript unsafe "$1.timeToDOMContentFlushed" js_get_timeToDOMContentFlushed
  :: PerformanceTiming -> (IO Word64)
