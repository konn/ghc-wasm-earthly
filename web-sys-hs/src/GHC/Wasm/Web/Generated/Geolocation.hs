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
module GHC.Wasm.Web.Generated.Geolocation (
        Geolocation, GeolocationClass,
        js_fun_getCurrentPosition_PositionCallback_nullable_nullable_PositionErrorCallback_nullable_PositionOptions_undefined,
        js_fun_watchPosition_PositionCallback_nullable_nullable_PositionErrorCallback_nullable_PositionOptions_long,
        js_fun_clearWatch_long_undefined
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Geolocation.Core
import GHC.Wasm.Web.Generated.PositionCallback.Core
import GHC.Wasm.Web.Generated.PositionErrorCallback.Core
import GHC.Wasm.Web.Generated.PositionOptions.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.getCurrentPosition($2,$3,$4)" js_fun_getCurrentPosition_PositionCallback_nullable_nullable_PositionErrorCallback_nullable_PositionOptions_undefined
  :: Geolocation
     -> (PositionCallback
         -> (Nullable (NullableClass PositionErrorCallbackClass)
             -> (Nullable PositionOptionsClass -> (IO ()))))
foreign import javascript unsafe "$1.watchPosition($2,$3,$4)" js_fun_watchPosition_PositionCallback_nullable_nullable_PositionErrorCallback_nullable_PositionOptions_long
  :: Geolocation
     -> (PositionCallback
         -> (Nullable (NullableClass PositionErrorCallbackClass)
             -> (Nullable PositionOptionsClass -> (IO Int32))))
foreign import javascript unsafe "$1.clearWatch($2)" js_fun_clearWatch_long_undefined
  :: Geolocation -> (Int32 -> (IO ()))
