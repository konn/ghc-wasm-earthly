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
module GHC.Wasm.Web.Generated.SourceBufferList (
        SourceBufferList, SourceBufferListClass, js_get_length,
        js_get_onaddsourcebuffer, js_set_onaddsourcebuffer,
        js_get_onremovesourcebuffer, js_set_onremovesourcebuffer
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.SourceBuffer.Core
import GHC.Wasm.Web.Generated.SourceBufferList.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.length" js_get_length
  :: SourceBufferList -> (IO Word32)
foreign import javascript unsafe "$1.onaddsourcebuffer" js_get_onaddsourcebuffer
  :: SourceBufferList -> (IO EventHandler)
foreign import javascript unsafe "$1.onaddsourcebuffer = $2" js_set_onaddsourcebuffer
  :: SourceBufferList -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onremovesourcebuffer" js_get_onremovesourcebuffer
  :: SourceBufferList -> (IO EventHandler)
foreign import javascript unsafe "$1.onremovesourcebuffer = $2" js_set_onremovesourcebuffer
  :: SourceBufferList -> (EventHandler -> (IO ()))
