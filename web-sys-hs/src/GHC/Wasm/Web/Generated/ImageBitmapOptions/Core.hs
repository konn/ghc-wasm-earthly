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
module GHC.Wasm.Web.Generated.ImageBitmapOptions.Core (
        ImageBitmapOptionsFields, ImageBitmapOptionsClass,
        ImageBitmapOptions, ReifiedImageBitmapOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ColorSpaceConversion.Core
import GHC.Wasm.Web.Generated.ImageOrientation.Core
import GHC.Wasm.Web.Generated.PremultiplyAlpha.Core
import GHC.Wasm.Web.Generated.ResizeQuality.Core
import GHC.Wasm.Web.Types
type ImageBitmapOptionsFields =
    '[ '("colorSpaceConversion",
         NullableClass ColorSpaceConversionClass),
       '("imageOrientation", NullableClass ImageOrientationClass),
       '("premultiplyAlpha", NullableClass PremultiplyAlphaClass),
       '("resizeHeight", NullableClass (JSPrimClass Word32)),
       '("resizeQuality", NullableClass ResizeQualityClass),
       '("resizeWidth", NullableClass (JSPrimClass Word32))]
type ImageBitmapOptionsClass =
    JSDictionaryClass ImageBitmapOptionsFields
type ImageBitmapOptions = JSObject ImageBitmapOptionsClass
type ReifiedImageBitmapOptions =
    ReifiedDictionary ImageBitmapOptionsFields
