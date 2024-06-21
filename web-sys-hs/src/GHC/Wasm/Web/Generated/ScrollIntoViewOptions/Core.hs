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
module GHC.Wasm.Web.Generated.ScrollIntoViewOptions.Core (
        ScrollIntoViewOptionsFields, ScrollIntoViewOptionsClass,
        ScrollIntoViewOptions, ReifiedScrollIntoViewOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ScrollBehavior.Core
import GHC.Wasm.Web.Generated.ScrollLogicalPosition.Core
import GHC.Wasm.Web.Types
type ScrollIntoViewOptionsFields =
    '[ '("behavior", NullableClass ScrollBehaviorClass),
       '("block", NullableClass ScrollLogicalPositionClass),
       '("inline", NullableClass ScrollLogicalPositionClass)]
type ScrollIntoViewOptionsClass =
    JSDictionaryClass ScrollIntoViewOptionsFields
type ScrollIntoViewOptions = JSObject ScrollIntoViewOptionsClass
type ReifiedScrollIntoViewOptions =
    ReifiedDictionary ScrollIntoViewOptionsFields
