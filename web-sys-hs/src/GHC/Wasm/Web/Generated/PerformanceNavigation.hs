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
module GHC.Wasm.Web.Generated.PerformanceNavigation (
        PerformanceNavigation, PerformanceNavigationClass,
        js_const_PerformanceNavigation_TYPE_NAVIGATE,
        js_const_PerformanceNavigation_TYPE_RELOAD,
        js_const_PerformanceNavigation_TYPE_BACK_FORWARD,
        js_const_PerformanceNavigation_TYPE_RESERVED,
        js_fun_toJSON__object, js_get_type, js_get_redirectCount
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.PerformanceNavigation.Core
import GHC.Wasm.Web.Types
js_const_PerformanceNavigation_TYPE_NAVIGATE :: Word16
js_const_PerformanceNavigation_TYPE_NAVIGATE = 0
js_const_PerformanceNavigation_TYPE_RELOAD :: Word16
js_const_PerformanceNavigation_TYPE_RELOAD = 1
js_const_PerformanceNavigation_TYPE_BACK_FORWARD :: Word16
js_const_PerformanceNavigation_TYPE_BACK_FORWARD = 2
js_const_PerformanceNavigation_TYPE_RESERVED :: Word16
js_const_PerformanceNavigation_TYPE_RESERVED = 255
foreign import javascript unsafe "$1.toJSON()" js_fun_toJSON__object
  :: PerformanceNavigation -> (IO JSAny)
foreign import javascript unsafe "$1.type" js_get_type
  :: PerformanceNavigation -> (IO Word16)
foreign import javascript unsafe "$1.redirectCount" js_get_redirectCount
  :: PerformanceNavigation -> (IO Word16)
