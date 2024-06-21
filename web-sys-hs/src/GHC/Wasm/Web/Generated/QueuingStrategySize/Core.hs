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
module GHC.Wasm.Web.Generated.QueuingStrategySize.Core (
        QueuingStrategySizeClass, QueuingStrategySize,
        js_mk_callback_QueuingStrategySize_impure
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type data QueuingStrategySizeClass :: Prototype
type instance SuperclassOf QueuingStrategySizeClass = 'Nothing
type QueuingStrategySize = JSObject QueuingStrategySizeClass
foreign import javascript unsafe "wrapper" js_mk_callback_QueuingStrategySize_impure
  :: (JSAny -> (IO Double)) -> QueuingStrategySize
