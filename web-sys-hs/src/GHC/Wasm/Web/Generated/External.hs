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
module GHC.Wasm.Web.Generated.External (
        External, ExternalClass,
        js_fun_AddSearchProvider_DOMString_undefined,
        js_fun_IsSearchProviderInstalled_DOMString_long
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.External.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.AddSearchProvider($2)" js_fun_AddSearchProvider_DOMString_undefined
  :: External -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.IsSearchProviderInstalled($2)" js_fun_IsSearchProviderInstalled_DOMString_long
  :: External -> (DOMString -> (IO Word32))
