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
module GHC.Wasm.Web.Generated.IdleDeadline (
        IdleDeadline, IdleDeadlineClass,
        js_fun_timeRemaining__DOMHighResTimeStamp, js_get_didTimeout
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMHighResTimeStamp.Core
import GHC.Wasm.Web.Generated.IdleDeadline.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.timeRemaining()" js_fun_timeRemaining__DOMHighResTimeStamp
  :: IdleDeadline -> (IO DOMHighResTimeStamp)
foreign import javascript unsafe "$1.didTimeout" js_get_didTimeout
  :: IdleDeadline -> (IO Bool)
