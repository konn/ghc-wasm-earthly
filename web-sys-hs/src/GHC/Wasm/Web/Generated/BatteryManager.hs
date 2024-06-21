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
module GHC.Wasm.Web.Generated.BatteryManager (
        BatteryManager, BatteryManagerClass, js_get_charging,
        js_get_chargingTime, js_get_dischargingTime, js_get_level,
        js_get_onchargingchange, js_set_onchargingchange,
        js_get_onchargingtimechange, js_set_onchargingtimechange,
        js_get_ondischargingtimechange, js_set_ondischargingtimechange,
        js_get_onlevelchange, js_set_onlevelchange
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.BatteryManager.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.charging" js_get_charging
  :: BatteryManager -> (IO Bool)
foreign import javascript unsafe "$1.chargingTime" js_get_chargingTime
  :: BatteryManager -> (IO Double)
foreign import javascript unsafe "$1.dischargingTime" js_get_dischargingTime
  :: BatteryManager -> (IO Double)
foreign import javascript unsafe "$1.level" js_get_level
  :: BatteryManager -> (IO Double)
foreign import javascript unsafe "$1.onchargingchange" js_get_onchargingchange
  :: BatteryManager -> (IO EventHandler)
foreign import javascript unsafe "$1.onchargingchange = $2" js_set_onchargingchange
  :: BatteryManager -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onchargingtimechange" js_get_onchargingtimechange
  :: BatteryManager -> (IO EventHandler)
foreign import javascript unsafe "$1.onchargingtimechange = $2" js_set_onchargingtimechange
  :: BatteryManager -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondischargingtimechange" js_get_ondischargingtimechange
  :: BatteryManager -> (IO EventHandler)
foreign import javascript unsafe "$1.ondischargingtimechange = $2" js_set_ondischargingtimechange
  :: BatteryManager -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onlevelchange" js_get_onlevelchange
  :: BatteryManager -> (IO EventHandler)
foreign import javascript unsafe "$1.onlevelchange = $2" js_set_onlevelchange
  :: BatteryManager -> (EventHandler -> (IO ()))
