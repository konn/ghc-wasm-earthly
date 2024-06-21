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
module GHC.Wasm.Web.Generated.ComputedEffectTiming.Core (
        ComputedEffectTimingFields, ComputedEffectTimingClass,
        ComputedEffectTiming, ReifiedComputedEffectTiming
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.FillMode.Core
import GHC.Wasm.Web.Generated.PlaybackDirection.Core
import GHC.Wasm.Web.Types
type ComputedEffectTimingFields =
    '[ '("activeDuration", NullableClass (JSPrimClass Double)),
       '("currentIteration",
         NullableClass (NullableClass (JSPrimClass Double))),
       '("delay", NullableClass (JSPrimClass Double)),
       '("direction", NullableClass PlaybackDirectionClass),
       '("duration",
         NullableClass (UnionClass '[JSPrimClass Double, DOMStringClass])),
       '("easing", NullableClass DOMStringClass),
       '("endDelay", NullableClass (JSPrimClass Double)),
       '("endTime", NullableClass (JSPrimClass Double)),
       '("fill", NullableClass FillModeClass),
       '("iterationStart", NullableClass (JSPrimClass Double)),
       '("iterations", NullableClass (JSPrimClass Double)),
       '("localTime", NullableClass (NullableClass (JSPrimClass Double))),
       '("progress", NullableClass (NullableClass (JSPrimClass Double)))]
type ComputedEffectTimingClass =
    JSDictionaryClass ComputedEffectTimingFields
type ComputedEffectTiming = JSObject ComputedEffectTimingClass
type ReifiedComputedEffectTiming =
    ReifiedDictionary ComputedEffectTimingFields
