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
module GHC.Wasm.Web.Generated.VRServiceTest (
        VRServiceTest, VRServiceTestClass,
        js_fun_attachVRDisplay_DOMString_Promise_VRMockDisplay,
        js_fun_attachVRController_DOMString_Promise_VRMockController
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.VRMockController.Core
import GHC.Wasm.Web.Generated.VRMockDisplay.Core
import GHC.Wasm.Web.Generated.VRServiceTest.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.attachVRDisplay($2)" js_fun_attachVRDisplay_DOMString_Promise_VRMockDisplay
  :: VRServiceTest
     -> (DOMString -> (IO (Promise VRMockDisplayClass)))
foreign import javascript safe "$1.attachVRController($2)" js_fun_attachVRController_DOMString_Promise_VRMockController
  :: VRServiceTest
     -> (DOMString -> (IO (Promise VRMockControllerClass)))
