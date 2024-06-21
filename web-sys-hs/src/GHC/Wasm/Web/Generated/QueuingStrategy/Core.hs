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
module GHC.Wasm.Web.Generated.QueuingStrategy.Core (
        QueuingStrategyFields, QueuingStrategyClass, QueuingStrategy,
        ReifiedQueuingStrategy
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.QueuingStrategySize.Core
import GHC.Wasm.Web.Types
type QueuingStrategyFields =
    '[ '("highWaterMark", NullableClass (JSPrimClass Double)),
       '("size", NullableClass QueuingStrategySizeClass)]
type QueuingStrategyClass = JSDictionaryClass QueuingStrategyFields
type QueuingStrategy = JSObject QueuingStrategyClass
type ReifiedQueuingStrategy =
    ReifiedDictionary QueuingStrategyFields
