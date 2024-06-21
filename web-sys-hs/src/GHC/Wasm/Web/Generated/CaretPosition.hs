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
module GHC.Wasm.Web.Generated.CaretPosition (
        CaretPosition, CaretPositionClass,
        js_fun_getClientRect__nullable_DOMRect, js_get_offsetNode,
        js_get_offset
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.CaretPosition.Core
import GHC.Wasm.Web.Generated.DOMRect.Core
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getClientRect()" js_fun_getClientRect__nullable_DOMRect
  :: CaretPosition -> (IO (Nullable DOMRectClass))
foreign import javascript unsafe "$1.offsetNode" js_get_offsetNode
  :: CaretPosition -> (IO (Nullable NodeClass))
foreign import javascript unsafe "$1.offset" js_get_offset
  :: CaretPosition -> (IO Word32)
