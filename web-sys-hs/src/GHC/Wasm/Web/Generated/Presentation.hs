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
module GHC.Wasm.Web.Generated.Presentation (
        Presentation, PresentationClass, js_get_defaultRequest,
        js_set_defaultRequest, js_get_receiver
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Presentation.Core
import GHC.Wasm.Web.Generated.PresentationReceiver.Core
import GHC.Wasm.Web.Generated.PresentationRequest.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.defaultRequest" js_get_defaultRequest
  :: Presentation -> (IO (Nullable PresentationRequestClass))
foreign import javascript unsafe "$1.defaultRequest = $2" js_set_defaultRequest
  :: Presentation -> (Nullable PresentationRequestClass -> (IO ()))
foreign import javascript unsafe "$1.receiver" js_get_receiver
  :: Presentation -> (IO (Nullable PresentationReceiverClass))
