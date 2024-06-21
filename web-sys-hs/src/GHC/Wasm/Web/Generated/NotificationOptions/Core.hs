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
module GHC.Wasm.Web.Generated.NotificationOptions.Core (
        NotificationOptionsFields, NotificationOptionsClass,
        NotificationOptions, ReifiedNotificationOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.NotificationAction.Core
import GHC.Wasm.Web.Generated.NotificationDirection.Core
import GHC.Wasm.Web.Types
type NotificationOptionsFields =
    '[ '("actions",
         NullableClass (SequenceClass NotificationActionClass)),
       '("badge", NullableClass USVStringClass),
       '("body", NullableClass DOMStringClass),
       '("data", NullableClass AnyClass),
       '("dir", NullableClass NotificationDirectionClass),
       '("icon", NullableClass USVStringClass),
       '("image", NullableClass USVStringClass),
       '("lang", NullableClass DOMStringClass),
       '("renotify", NullableClass (JSPrimClass Bool)),
       '("requireInteraction", NullableClass (JSPrimClass Bool)),
       '("silent", NullableClass (NullableClass (JSPrimClass Bool))),
       '("tag", NullableClass DOMStringClass),
       '("timestamp", NullableClass (JSPrimClass Word64))]
type NotificationOptionsClass =
    JSDictionaryClass NotificationOptionsFields
type NotificationOptions = JSObject NotificationOptionsClass
type ReifiedNotificationOptions =
    ReifiedDictionary NotificationOptionsFields
