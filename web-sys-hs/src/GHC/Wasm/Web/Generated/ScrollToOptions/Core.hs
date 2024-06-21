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
module GHC.Wasm.Web.Generated.ScrollToOptions.Core (
        ScrollToOptionsFields, ScrollToOptionsClass, ScrollToOptions,
        ReifiedScrollToOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ScrollBehavior.Core
import GHC.Wasm.Web.Types
type ScrollToOptionsFields =
    '[ '("behavior", NullableClass ScrollBehaviorClass),
       '("left", NullableClass (JSPrimClass Double)),
       '("top", NullableClass (JSPrimClass Double))]
type ScrollToOptionsClass = JSDictionaryClass ScrollToOptionsFields
type ScrollToOptions = JSObject ScrollToOptionsClass
type ReifiedScrollToOptions =
    ReifiedDictionary ScrollToOptionsFields
