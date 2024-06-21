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
module GHC.Wasm.Web.Generated.RegisteredKey.Core (
        RegisteredKeyFields, RegisteredKeyClass, RegisteredKey,
        ReifiedRegisteredKey
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Transports.Core
import GHC.Wasm.Web.Types
type RegisteredKeyFields =
    '[ '("appId", NullableClass (NullableClass DOMStringClass)),
       '("keyHandle", NullableClass DOMStringClass),
       '("transports", NullableClass (NullableClass TransportsClass)),
       '("version", NullableClass DOMStringClass)]
type RegisteredKeyClass = JSDictionaryClass RegisteredKeyFields
type RegisteredKey = JSObject RegisteredKeyClass
type ReifiedRegisteredKey = ReifiedDictionary RegisteredKeyFields
