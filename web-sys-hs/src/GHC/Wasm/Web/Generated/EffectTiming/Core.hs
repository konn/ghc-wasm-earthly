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
module GHC.Wasm.Web.Generated.EffectTiming.Core (
        EffectTimingFields, EffectTimingClass, EffectTiming,
        ReifiedEffectTiming
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.FillMode.Core
import GHC.Wasm.Web.Generated.PlaybackDirection.Core
import GHC.Wasm.Web.Types
type EffectTimingFields =
    '[ '("delay", NullableClass (JSPrimClass Double)),
       '("direction", NullableClass PlaybackDirectionClass),
       '("duration",
         NullableClass (UnionClass '[JSPrimClass Double, DOMStringClass])),
       '("easing", NullableClass DOMStringClass),
       '("endDelay", NullableClass (JSPrimClass Double)),
       '("fill", NullableClass FillModeClass),
       '("iterationStart", NullableClass (JSPrimClass Double)),
       '("iterations", NullableClass (JSPrimClass Double))]
type EffectTimingClass = JSDictionaryClass EffectTimingFields
type EffectTiming = JSObject EffectTimingClass
type ReifiedEffectTiming = ReifiedDictionary EffectTimingFields
