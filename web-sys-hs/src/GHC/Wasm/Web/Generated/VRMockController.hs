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
module GHC.Wasm.Web.Generated.VRMockController (
        VRMockController, VRMockControllerClass,
        js_fun_newButtonEvent_long_boolean_undefined,
        js_fun_newAxisMoveEvent_long_double_undefined,
        js_fun_newPoseMove_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_undefined
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.VRMockController.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.newButtonEvent($2,$3)" js_fun_newButtonEvent_long_boolean_undefined
  :: VRMockController -> (Word32 -> (Bool -> (IO ())))
foreign import javascript unsafe "$1.newAxisMoveEvent($2,$3)" js_fun_newAxisMoveEvent_long_double_undefined
  :: VRMockController -> (Word32 -> (Double -> (IO ())))
foreign import javascript unsafe "$1.newPoseMove($2,$3,$4,$5,$6,$7)" js_fun_newPoseMove_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_undefined
  :: VRMockController
     -> (Nullable (JSByteArrayClass Float)
         -> (Nullable (JSByteArrayClass Float)
             -> (Nullable (JSByteArrayClass Float)
                 -> (Nullable (JSByteArrayClass Float)
                     -> (Nullable (JSByteArrayClass Float)
                         -> (Nullable (JSByteArrayClass Float) -> (IO ())))))))
