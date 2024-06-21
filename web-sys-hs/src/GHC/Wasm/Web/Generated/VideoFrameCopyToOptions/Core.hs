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
module GHC.Wasm.Web.Generated.VideoFrameCopyToOptions.Core (
        VideoFrameCopyToOptionsFields, VideoFrameCopyToOptionsClass,
        VideoFrameCopyToOptions, ReifiedVideoFrameCopyToOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMRectInit.Core
import GHC.Wasm.Web.Generated.PlaneLayout.Core
import GHC.Wasm.Web.Types
type VideoFrameCopyToOptionsFields =
    '[ '("layout", NullableClass (SequenceClass PlaneLayoutClass)),
       '("rect", NullableClass DOMRectInitClass)]
type VideoFrameCopyToOptionsClass =
    JSDictionaryClass VideoFrameCopyToOptionsFields
type VideoFrameCopyToOptions =
    JSObject VideoFrameCopyToOptionsClass
type ReifiedVideoFrameCopyToOptions =
    ReifiedDictionary VideoFrameCopyToOptionsFields
