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
module GHC.Wasm.Web.Generated.ConvertCoordinateOptions.Core (
        ConvertCoordinateOptionsFields, ConvertCoordinateOptionsClass,
        ConvertCoordinateOptions, ReifiedConvertCoordinateOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.CSSBoxType.Core
import GHC.Wasm.Web.Types
type ConvertCoordinateOptionsFields =
    '[ '("fromBox", NullableClass CSSBoxTypeClass),
       '("toBox", NullableClass CSSBoxTypeClass)]
type ConvertCoordinateOptionsClass =
    JSDictionaryClass ConvertCoordinateOptionsFields
type ConvertCoordinateOptions =
    JSObject ConvertCoordinateOptionsClass
type ReifiedConvertCoordinateOptions =
    ReifiedDictionary ConvertCoordinateOptionsFields
