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
module GHC.Wasm.Web.Generated.AnimationEffect (
        AnimationEffect, AnimationEffectClass,
        js_fun_getTiming__EffectTiming,
        js_fun_getComputedTiming__ComputedEffectTiming,
        js_fun_updateTiming_nullable_OptionalEffectTiming_undefined
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AnimationEffect.Core
import GHC.Wasm.Web.Generated.ComputedEffectTiming.Core
import GHC.Wasm.Web.Generated.EffectTiming.Core
import GHC.Wasm.Web.Generated.OptionalEffectTiming.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getTiming()" js_fun_getTiming__EffectTiming
  :: AnimationEffect -> (IO EffectTiming)
foreign import javascript unsafe "$1.getComputedTiming()" js_fun_getComputedTiming__ComputedEffectTiming
  :: AnimationEffect -> (IO ComputedEffectTiming)
foreign import javascript unsafe "$1.updateTiming($2)" js_fun_updateTiming_nullable_OptionalEffectTiming_undefined
  :: AnimationEffect
     -> (Nullable OptionalEffectTimingClass -> (IO ()))
