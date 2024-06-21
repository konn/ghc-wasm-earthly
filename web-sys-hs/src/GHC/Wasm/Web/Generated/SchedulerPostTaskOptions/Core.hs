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
module GHC.Wasm.Web.Generated.SchedulerPostTaskOptions.Core (
        SchedulerPostTaskOptionsFields, SchedulerPostTaskOptionsClass,
        SchedulerPostTaskOptions, ReifiedSchedulerPostTaskOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AbortSignal.Core
import GHC.Wasm.Web.Generated.TaskPriority.Core
import GHC.Wasm.Web.Types
type SchedulerPostTaskOptionsFields =
    '[ '("delay", NullableClass (JSPrimClass Word64)),
       '("priority", NullableClass TaskPriorityClass),
       '("signal", NullableClass AbortSignalClass)]
type SchedulerPostTaskOptionsClass =
    JSDictionaryClass SchedulerPostTaskOptionsFields
type SchedulerPostTaskOptions =
    JSObject SchedulerPostTaskOptionsClass
type ReifiedSchedulerPostTaskOptions =
    ReifiedDictionary SchedulerPostTaskOptionsFields
