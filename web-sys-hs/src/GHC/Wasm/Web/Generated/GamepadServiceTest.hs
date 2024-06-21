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
module GHC.Wasm.Web.Generated.GamepadServiceTest (
        GamepadServiceTest, GamepadServiceTestClass,
        js_fun_addGamepad_DOMString_GamepadMappingType_GamepadHand_long_long_long_Promise_long,
        js_fun_removeGamepad_long_undefined,
        js_fun_newButtonEvent_long_long_boolean_boolean_undefined,
        js_fun_newButtonValueEvent_long_long_boolean_boolean_double_undefined,
        js_fun_newAxisMoveEvent_long_long_double_undefined,
        js_fun_newPoseMove_long_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_undefined,
        js_get_noMapping, js_get_standardMapping, js_get_noHand,
        js_get_leftHand, js_get_rightHand
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.GamepadHand.Core
import GHC.Wasm.Web.Generated.GamepadMappingType.Core
import GHC.Wasm.Web.Generated.GamepadServiceTest.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.addGamepad($2,$3,$4,$5,$6,$7)" js_fun_addGamepad_DOMString_GamepadMappingType_GamepadHand_long_long_long_Promise_long
  :: GamepadServiceTest
     -> (DOMString
         -> (GamepadMappingType
             -> (GamepadHand
                 -> (Word32
                     -> (Word32 -> (Word32 -> (IO (Promise (JSPrimClass Word32)))))))))
foreign import javascript unsafe "$1.removeGamepad($2)" js_fun_removeGamepad_long_undefined
  :: GamepadServiceTest -> (Word32 -> (IO ()))
foreign import javascript unsafe "$1.newButtonEvent($2,$3,$4,$5)" js_fun_newButtonEvent_long_long_boolean_boolean_undefined
  :: GamepadServiceTest
     -> (Word32 -> (Word32 -> (Bool -> (Bool -> (IO ())))))
foreign import javascript unsafe "$1.newButtonValueEvent($2,$3,$4,$5,$6)" js_fun_newButtonValueEvent_long_long_boolean_boolean_double_undefined
  :: GamepadServiceTest
     -> (Word32 -> (Word32 -> (Bool -> (Bool -> (Double -> (IO ()))))))
foreign import javascript unsafe "$1.newAxisMoveEvent($2,$3,$4)" js_fun_newAxisMoveEvent_long_long_double_undefined
  :: GamepadServiceTest
     -> (Word32 -> (Word32 -> (Double -> (IO ()))))
foreign import javascript unsafe "$1.newPoseMove($2,$3,$4,$5,$6,$7,$8)" js_fun_newPoseMove_long_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_nullable_Float32Array_undefined
  :: GamepadServiceTest
     -> (Word32
         -> (Nullable (JSByteArrayClass Float)
             -> (Nullable (JSByteArrayClass Float)
                 -> (Nullable (JSByteArrayClass Float)
                     -> (Nullable (JSByteArrayClass Float)
                         -> (Nullable (JSByteArrayClass Float)
                             -> (Nullable (JSByteArrayClass Float) -> (IO ()))))))))
foreign import javascript unsafe "$1.noMapping" js_get_noMapping
  :: GamepadServiceTest -> (IO GamepadMappingType)
foreign import javascript unsafe "$1.standardMapping" js_get_standardMapping
  :: GamepadServiceTest -> (IO GamepadMappingType)
foreign import javascript unsafe "$1.noHand" js_get_noHand
  :: GamepadServiceTest -> (IO GamepadHand)
foreign import javascript unsafe "$1.leftHand" js_get_leftHand
  :: GamepadServiceTest -> (IO GamepadHand)
foreign import javascript unsafe "$1.rightHand" js_get_rightHand
  :: GamepadServiceTest -> (IO GamepadHand)
