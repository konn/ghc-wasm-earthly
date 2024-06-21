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
module GHC.Wasm.Web.Generated.MediaSessionActionHandler.Core (
        MediaSessionActionHandlerClass, MediaSessionActionHandler,
        js_mk_callback_MediaSessionActionHandler_impure
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.MediaSessionActionDetails.Core
import GHC.Wasm.Web.Types
type data MediaSessionActionHandlerClass :: Prototype
type instance SuperclassOf MediaSessionActionHandlerClass = 'Nothing
type MediaSessionActionHandler =
    JSObject MediaSessionActionHandlerClass
foreign import javascript unsafe "wrapper" js_mk_callback_MediaSessionActionHandler_impure
  :: (MediaSessionActionDetails -> (IO ()))
     -> MediaSessionActionHandler
