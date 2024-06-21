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
module GHC.Wasm.Web.Generated.OnBeforeUnloadEventHandlerNonNull.Core (
        OnBeforeUnloadEventHandlerNonNullClass,
        OnBeforeUnloadEventHandlerNonNull,
        js_mk_callback_OnBeforeUnloadEventHandlerNonNull_impure
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Event.Core
import GHC.Wasm.Web.Types
type data OnBeforeUnloadEventHandlerNonNullClass :: Prototype
type instance SuperclassOf OnBeforeUnloadEventHandlerNonNullClass = 'Nothing
type OnBeforeUnloadEventHandlerNonNull =
    JSObject OnBeforeUnloadEventHandlerNonNullClass
foreign import javascript unsafe "wrapper" js_mk_callback_OnBeforeUnloadEventHandlerNonNull_impure
  :: (Event -> (IO (Nullable DOMStringClass)))
     -> OnBeforeUnloadEventHandlerNonNull
