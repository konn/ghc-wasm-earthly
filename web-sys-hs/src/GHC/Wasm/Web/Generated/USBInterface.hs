{-# OPTIONS_GHC -Wno-unused-imports #-}
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
module GHC.Wasm.Web.Generated.USBInterface (
        USBInterface, USBInterfaceClass, js_cons_USBInterface,
        js_get_interfaceNumber, js_get_alternate, js_get_alternates,
        js_get_claimed
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.USBAlternateInterface.Core
import GHC.Wasm.Web.Generated.USBConfiguration.Core
import GHC.Wasm.Web.Generated.USBInterface.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "USBInterface($1,$2)" js_cons_USBInterface
  :: USBConfiguration -> (Word8 -> (IO USBInterface))
foreign import javascript unsafe "$1.interfaceNumber" js_get_interfaceNumber
  :: USBInterface -> (IO Word8)
foreign import javascript unsafe "$1.alternate" js_get_alternate
  :: USBInterface -> (IO USBAlternateInterface)
foreign import javascript unsafe "$1.alternates" js_get_alternates
  :: USBInterface -> (IO (FrozenArray USBAlternateInterfaceClass))
foreign import javascript unsafe "$1.claimed" js_get_claimed
  :: USBInterface -> (IO Bool)
