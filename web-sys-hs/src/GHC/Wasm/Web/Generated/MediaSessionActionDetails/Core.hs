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
module GHC.Wasm.Web.Generated.MediaSessionActionDetails.Core (
        MediaSessionActionDetailsFields, MediaSessionActionDetailsClass,
        MediaSessionActionDetails, ReifiedMediaSessionActionDetails
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MediaSessionAction.Core
import GHC.Wasm.Web.Types
type MediaSessionActionDetailsFields =
    '[ '("action", MediaSessionActionClass),
       '("fastSeek", NullableClass (NullableClass (JSPrimClass Bool))),
       '("seekOffset",
         NullableClass (NullableClass (JSPrimClass Double))),
       '("seekTime", NullableClass (NullableClass (JSPrimClass Double)))]
type MediaSessionActionDetailsClass =
    JSDictionaryClass MediaSessionActionDetailsFields
type MediaSessionActionDetails =
    JSObject MediaSessionActionDetailsClass
type ReifiedMediaSessionActionDetails =
    ReifiedDictionary MediaSessionActionDetailsFields
