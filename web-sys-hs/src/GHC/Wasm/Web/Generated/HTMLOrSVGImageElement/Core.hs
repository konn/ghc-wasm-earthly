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
module GHC.Wasm.Web.Generated.HTMLOrSVGImageElement.Core (
        HTMLOrSVGImageElementClass, HTMLOrSVGImageElement
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.HTMLImageElement.Core
import GHC.Wasm.Web.Generated.SVGImageElement.Core
import GHC.Wasm.Web.Types
type HTMLOrSVGImageElementClass =
    UnionClass '[HTMLImageElementClass, SVGImageElementClass]
type HTMLOrSVGImageElement =
    JSObject (UnionClass '[HTMLImageElementClass,
                           SVGImageElementClass])
