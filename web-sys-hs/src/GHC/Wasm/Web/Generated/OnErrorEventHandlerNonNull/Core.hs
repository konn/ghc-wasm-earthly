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
module GHC.Wasm.Web.Generated.OnErrorEventHandlerNonNull.Core (
        OnErrorEventHandlerNonNullClass, OnErrorEventHandlerNonNull,
        js_mk_callback_OnErrorEventHandlerNonNull_impure
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Event.Core
import GHC.Wasm.Web.Types
type data OnErrorEventHandlerNonNullClass :: Prototype
type instance SuperclassOf OnErrorEventHandlerNonNullClass = 'Nothing
type OnErrorEventHandlerNonNull =
    JSObject OnErrorEventHandlerNonNullClass
foreign import javascript unsafe "wrapper" js_mk_callback_OnErrorEventHandlerNonNull_impure
  :: (JSObject (UnionClass '[EventClass, DOMStringClass])
      -> (Nullable DOMStringClass
          -> (Nullable (JSPrimClass Word32)
              -> (Nullable (JSPrimClass Word32)
                  -> (Nullable AnyClass -> (IO JSAny))))))
     -> OnErrorEventHandlerNonNull
