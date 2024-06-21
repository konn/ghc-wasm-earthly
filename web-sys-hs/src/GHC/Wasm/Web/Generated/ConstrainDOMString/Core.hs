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
module GHC.Wasm.Web.Generated.ConstrainDOMString.Core (
        ConstrainDOMStringClass, ConstrainDOMString
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ConstrainDOMStringParameters.Core
import GHC.Wasm.Web.Types
type ConstrainDOMStringClass =
    UnionClass '[DOMStringClass,
                 SequenceClass DOMStringClass,
                 ConstrainDOMStringParametersClass]
type ConstrainDOMString =
    JSObject (UnionClass '[DOMStringClass,
                           SequenceClass DOMStringClass,
                           ConstrainDOMStringParametersClass])
