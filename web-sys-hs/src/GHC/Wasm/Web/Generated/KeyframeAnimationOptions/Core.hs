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
module GHC.Wasm.Web.Generated.KeyframeAnimationOptions.Core (
        KeyframeAnimationOptionsFields, KeyframeAnimationOptionsClass,
        KeyframeAnimationOptions, ReifiedKeyframeAnimationOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AnimationTimeline.Core
import GHC.Wasm.Web.Generated.CompositeOperation.Core
import GHC.Wasm.Web.Generated.IterationCompositeOperation.Core
import GHC.Wasm.Web.Types
type KeyframeAnimationOptionsFields =
    '[ '("composite", NullableClass CompositeOperationClass),
       '("id", NullableClass DOMStringClass),
       '("iterationComposite",
         NullableClass IterationCompositeOperationClass),
       '("timeline",
         NullableClass (NullableClass AnimationTimelineClass))]
type KeyframeAnimationOptionsClass =
    JSDictionaryClass KeyframeAnimationOptionsFields
type KeyframeAnimationOptions =
    JSObject KeyframeAnimationOptionsClass
type ReifiedKeyframeAnimationOptions =
    ReifiedDictionary KeyframeAnimationOptionsFields
