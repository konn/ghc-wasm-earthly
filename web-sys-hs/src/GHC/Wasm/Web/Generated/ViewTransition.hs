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
module GHC.Wasm.Web.Generated.ViewTransition (
        ViewTransition, ViewTransitionClass,
        js_fun_skipTransition__undefined, js_get_updateCallbackDone,
        js_get_ready, js_get_finished
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ViewTransition.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.skipTransition()" js_fun_skipTransition__undefined
  :: ViewTransition -> (IO ())
foreign import javascript unsafe "$1.updateCallbackDone" js_get_updateCallbackDone
  :: ViewTransition -> (IO (Promise UndefinedClass))
foreign import javascript unsafe "$1.ready" js_get_ready
  :: ViewTransition -> (IO (Promise UndefinedClass))
foreign import javascript unsafe "$1.finished" js_get_finished
  :: ViewTransition -> (IO (Promise UndefinedClass))
