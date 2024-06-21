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
module GHC.Wasm.Web.Generated.FontFaceDescriptors.Core (
        FontFaceDescriptorsFields, FontFaceDescriptorsClass,
        FontFaceDescriptors, ReifiedFontFaceDescriptors
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type FontFaceDescriptorsFields =
    '[ '("display", NullableClass DOMStringClass),
       '("featureSettings", NullableClass DOMStringClass),
       '("stretch", NullableClass DOMStringClass),
       '("style", NullableClass DOMStringClass),
       '("unicodeRange", NullableClass DOMStringClass),
       '("variant", NullableClass DOMStringClass),
       '("variationSettings", NullableClass DOMStringClass),
       '("weight", NullableClass DOMStringClass)]
type FontFaceDescriptorsClass =
    JSDictionaryClass FontFaceDescriptorsFields
type FontFaceDescriptors = JSObject FontFaceDescriptorsClass
type ReifiedFontFaceDescriptors =
    ReifiedDictionary FontFaceDescriptorsFields
