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
module GHC.Wasm.Web.Generated.CryptoKey (
        CryptoKey, CryptoKeyClass, js_get_type, js_get_extractable,
        js_get_algorithm, js_get_usages
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.CryptoKey.Core
import GHC.Wasm.Web.Generated.KeyType.Core
import GHC.Wasm.Web.Generated.KeyUsage.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.type" js_get_type
  :: CryptoKey -> (IO KeyType)
foreign import javascript unsafe "$1.extractable" js_get_extractable
  :: CryptoKey -> (IO Bool)
foreign import javascript unsafe "$1.algorithm" js_get_algorithm
  :: CryptoKey -> (IO JSAny)
foreign import javascript unsafe "$1.usages" js_get_usages
  :: CryptoKey -> (IO (Sequence KeyUsageClass))
