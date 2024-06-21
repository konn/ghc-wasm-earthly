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
module GHC.Wasm.Web.Generated.VRMockDisplay (
        VRMockDisplay, VRMockDisplayClass,
        js_fun_setEyeResolution_long_long_undefined,
        js_fun_setEyeParameter_VREye_double_double_double_double_double_double_double_undefined,
        js_fun_setPose_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_undefined,
        js_fun_setMountState_boolean_undefined, js_fun_update__undefined
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.VREye.Core
import GHC.Wasm.Web.Generated.VRMockDisplay.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.setEyeResolution($2,$3)" js_fun_setEyeResolution_long_long_undefined
  :: VRMockDisplay -> (Word32 -> (Word32 -> (IO ())))
foreign import javascript unsafe "$1.setEyeParameter($2,$3,$4,$5,$6,$7,$8,$9)" js_fun_setEyeParameter_VREye_double_double_double_double_double_double_double_undefined
  :: VRMockDisplay
     -> (VREye
         -> (Double
             -> (Double
                 -> (Double
                     -> (Double -> (Double -> (Double -> (Double -> (IO ())))))))))
foreign import javascript unsafe "$1.setPose($2,$3,$4,$5,$6,$7)" js_fun_setPose_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_undefined
  :: VRMockDisplay
     -> (Nullable (JSByteArrayClass Float)
         -> (Nullable (JSByteArrayClass Float)
             -> (Nullable (JSByteArrayClass Float)
                 -> (Nullable (JSByteArrayClass Float)
                     -> (Nullable (JSByteArrayClass Float)
                         -> (Nullable (JSByteArrayClass Float) -> (IO ())))))))
foreign import javascript unsafe "$1.setMountState($2)" js_fun_setMountState_boolean_undefined
  :: VRMockDisplay -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.update()" js_fun_update__undefined
  :: VRMockDisplay -> (IO ())
