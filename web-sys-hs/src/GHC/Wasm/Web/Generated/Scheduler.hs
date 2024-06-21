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
module GHC.Wasm.Web.Generated.Scheduler (
        Scheduler, SchedulerClass,
        js_fun_postTask_SchedulerPostTaskCallback_nullable_SchedulerPostTaskOptions_Promise_any
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Scheduler.Core
import GHC.Wasm.Web.Generated.SchedulerPostTaskCallback.Core
import GHC.Wasm.Web.Generated.SchedulerPostTaskOptions.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.postTask($2,$3)" js_fun_postTask_SchedulerPostTaskCallback_nullable_SchedulerPostTaskOptions_Promise_any
  :: Scheduler
     -> (SchedulerPostTaskCallback
         -> (Nullable SchedulerPostTaskOptionsClass
             -> (IO (Promise AnyClass))))
