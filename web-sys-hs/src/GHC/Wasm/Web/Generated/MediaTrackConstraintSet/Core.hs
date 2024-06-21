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
module GHC.Wasm.Web.Generated.MediaTrackConstraintSet.Core (
        MediaTrackConstraintSetFields, MediaTrackConstraintSetClass,
        MediaTrackConstraintSet, ReifiedMediaTrackConstraintSet
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ConstrainBoolean.Core
import GHC.Wasm.Web.Generated.ConstrainDOMString.Core
import GHC.Wasm.Web.Generated.ConstrainDouble.Core
import GHC.Wasm.Web.Generated.ConstrainLong.Core
import GHC.Wasm.Web.Types
type MediaTrackConstraintSetFields =
    '[ '("autoGainControl", NullableClass ConstrainBooleanClass),
       '("browserWindow", NullableClass (JSPrimClass Int64)),
       '("channelCount", NullableClass ConstrainLongClass),
       '("deviceId", NullableClass ConstrainDOMStringClass),
       '("echoCancellation", NullableClass ConstrainBooleanClass),
       '("facingMode", NullableClass ConstrainDOMStringClass),
       '("frameRate", NullableClass ConstrainDoubleClass),
       '("height", NullableClass ConstrainLongClass),
       '("mediaSource", NullableClass DOMStringClass),
       '("noiseSuppression", NullableClass ConstrainBooleanClass),
       '("scrollWithPage", NullableClass (JSPrimClass Bool)),
       '("viewportHeight", NullableClass ConstrainLongClass),
       '("viewportOffsetX", NullableClass ConstrainLongClass),
       '("viewportOffsetY", NullableClass ConstrainLongClass),
       '("viewportWidth", NullableClass ConstrainLongClass),
       '("width", NullableClass ConstrainLongClass)]
type MediaTrackConstraintSetClass =
    JSDictionaryClass MediaTrackConstraintSetFields
type MediaTrackConstraintSet =
    JSObject MediaTrackConstraintSetClass
type ReifiedMediaTrackConstraintSet =
    ReifiedDictionary MediaTrackConstraintSetFields
