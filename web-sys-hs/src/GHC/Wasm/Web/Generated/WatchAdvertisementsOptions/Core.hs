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
module GHC.Wasm.Web.Generated.WatchAdvertisementsOptions.Core (
        WatchAdvertisementsOptionsFields, WatchAdvertisementsOptionsClass,
        WatchAdvertisementsOptions, ReifiedWatchAdvertisementsOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AbortSignal.Core
import GHC.Wasm.Web.Types
type WatchAdvertisementsOptionsFields =
    '[ '("signal", NullableClass AbortSignalClass)]
type WatchAdvertisementsOptionsClass =
    JSDictionaryClass WatchAdvertisementsOptionsFields
type WatchAdvertisementsOptions =
    JSObject WatchAdvertisementsOptionsClass
type ReifiedWatchAdvertisementsOptions =
    ReifiedDictionary WatchAdvertisementsOptionsFields
