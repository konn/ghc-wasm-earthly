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
module GHC.Wasm.Web.Generated.EventHandlerNonNull.Core (
        EventHandlerNonNullClass, EventHandlerNonNull,
        js_mk_callback_EventHandlerNonNull_impure
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Event.Core
import GHC.Wasm.Web.Types
type data EventHandlerNonNullClass :: Prototype
type instance SuperclassOf EventHandlerNonNullClass = 'Nothing
type EventHandlerNonNull = JSObject EventHandlerNonNullClass
foreign import javascript unsafe "wrapper" js_mk_callback_EventHandlerNonNull_impure
  :: (Event -> (IO JSAny)) -> EventHandlerNonNull
