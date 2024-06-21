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
module GHC.Wasm.Web.Generated.VRSubmitFrameResult (
        VRSubmitFrameResult, VRSubmitFrameResultClass, js_get_frameNum,
        js_get_base64Image
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.VRSubmitFrameResult.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.frameNum" js_get_frameNum
  :: VRSubmitFrameResult -> (IO Word32)
foreign import javascript unsafe "$1.base64Image" js_get_base64Image
  :: VRSubmitFrameResult -> (IO (Nullable DOMStringClass))
