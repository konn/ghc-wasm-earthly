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
module GHC.Wasm.Web.Generated.BodyInit.Core (
        BodyInitClass, BodyInit
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Blob.Core
import GHC.Wasm.Web.Generated.FormData.Core
import GHC.Wasm.Web.Generated.ReadableStream.Core
import GHC.Wasm.Web.Generated.URLSearchParams.Core
import GHC.Wasm.Web.Types
type BodyInitClass =
    UnionClass '[BlobClass,
                 BufferSourceClass,
                 FormDataClass,
                 URLSearchParamsClass,
                 USVStringClass,
                 ReadableStreamClass]
type BodyInit =
    JSObject (UnionClass '[BlobClass,
                           BufferSourceClass,
                           FormDataClass,
                           URLSearchParamsClass,
                           USVStringClass,
                           ReadableStreamClass])
