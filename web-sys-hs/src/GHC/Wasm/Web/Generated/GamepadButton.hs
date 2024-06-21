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
module GHC.Wasm.Web.Generated.GamepadButton (
        GamepadButton, GamepadButtonClass, js_get_pressed, js_get_touched,
        js_get_value
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.GamepadButton.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.pressed" js_get_pressed
  :: GamepadButton -> (IO Bool)
foreign import javascript unsafe "$1.touched" js_get_touched
  :: GamepadButton -> (IO Bool)
foreign import javascript unsafe "$1.value" js_get_value
  :: GamepadButton -> (IO Double)
