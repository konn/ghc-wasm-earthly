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
module GHC.Wasm.Web.Generated.VideoColorSpaceInit.Core (
        VideoColorSpaceInitFields, VideoColorSpaceInitClass,
        VideoColorSpaceInit, ReifiedVideoColorSpaceInit
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.VideoColorPrimaries.Core
import GHC.Wasm.Web.Generated.VideoMatrixCoefficients.Core
import GHC.Wasm.Web.Generated.VideoTransferCharacteristics.Core
import GHC.Wasm.Web.Types
type VideoColorSpaceInitFields =
    '[ '("fullRange", NullableClass (JSPrimClass Bool)),
       '("matrix", NullableClass VideoMatrixCoefficientsClass),
       '("primaries", NullableClass VideoColorPrimariesClass),
       '("transfer", NullableClass VideoTransferCharacteristicsClass)]
type VideoColorSpaceInitClass =
    JSDictionaryClass VideoColorSpaceInitFields
type VideoColorSpaceInit = JSObject VideoColorSpaceInitClass
type ReifiedVideoColorSpaceInit =
    ReifiedDictionary VideoColorSpaceInitFields
