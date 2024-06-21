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
module GHC.Wasm.Web.Generated.WakeLock (
        WakeLock, WakeLockClass,
        js_fun_request_WakeLockType_Promise_WakeLockSentinel
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.WakeLock.Core
import GHC.Wasm.Web.Generated.WakeLockSentinel.Core
import GHC.Wasm.Web.Generated.WakeLockType.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.request($2)" js_fun_request_WakeLockType_Promise_WakeLockSentinel
  :: WakeLock
     -> (WakeLockType -> (IO (Promise WakeLockSentinelClass)))
